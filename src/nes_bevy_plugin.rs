use std::default;

use crate::nes::{
    self, NES_PATTERN_TABLE_HEIGHT, NES_PATTERN_TABLE_WIDTH, NES_SCREEN_HEIGHT, NES_SCREEN_WIDTH,
    NUM_COLOURS_IN_NES_PALETTE, NUM_NES_COLOUR_PALETTES, NUM_NES_PATTERN_TABLES,
};

use super::nes::Nes;
use bevy::{
    core_pipeline::core_2d::graph::input,
    prelude::{default, *},
    render::{
        render_asset::{RenderAssetUsages, RenderAssets},
        render_graph::{self, NodeRunError, RenderGraph, RenderGraphContext, RenderLabel},
        render_resource::{
            Buffer, BufferInitDescriptor, CommandEncoderDescriptor, Extent3d, ImageCopyBuffer,
            ImageDataLayout, TextureDimension, TextureFormat,
        },
        renderer::{RenderContext, RenderDevice},
        texture::BevyDefault,
        RenderApp,
    },
    text::BreakLineOn,
};
use crossbeam_channel::{Receiver, Sender};
use leafwing_input_manager::{action_state, input_map, prelude::*};

static NES_TEST: &'static [u8] = include_bytes!("../assets/roms/nestest.nes");

const NES_PALETTE_SWATCH_PIXEL_WIDTH: usize = 64;

#[derive(Actionlike, Reflect, Clone, Hash, Eq, PartialEq)]
enum NesInput {
    A,
    B,
    Select,
    Start,
    Dpad,
}

#[derive(Actionlike, Reflect, Clone, Hash, Eq, PartialEq)]
enum NesDebuggingActions {
    RunEmulation,
    PauseEmulation,
    StepInstruction,
    StepFrame,
    PrevMemoryPage,
    NextMemoryPage,
    PrevColourPalette,
    NextColourPalette,
}

#[derive(Debug, Default, States, Hash, Eq, PartialEq, Clone)]
enum EmulationState {
    #[default]
    Running,
    RunOnce,
    StepOnce,
    Paused,
}

pub struct NesPlugin;

impl Plugin for NesPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(InputManagerPlugin::<NesInput>::default())
            .add_plugins(InputManagerPlugin::<NesDebuggingActions>::default())
            .add_plugins(EmulatorScreenPlugin)
            .insert_resource(Time::<Fixed>::from_hz(60.0))
            .insert_state(EmulationState::default())
            .add_systems(Startup, setup_nes)
            .add_systems(
                FixedUpdate,
                (
                    (
                        nes_frame.run_if(
                            move |current_state: Option<Res<State<EmulationState>>>| {
                                match current_state {
                                    Some(current_state) => {
                                        *current_state == EmulationState::Running
                                            || *current_state == EmulationState::RunOnce
                                    }
                                    None => false,
                                }
                            },
                        ),
                        nes_paused.run_if(in_state(EmulationState::Paused)),
                    ),
                    nes_pause_from_run_once.run_if(in_state(EmulationState::RunOnce)),
                    nes_pause_from_running.run_if(in_state(EmulationState::Running)),
                    nes_debugging_show_emulation_state,
                    nes_debugging_show_emulator_frame_counter,
                    nes_debugging_pattern_table_visualization,
                    nes_debugging_colour_palette_visualization,
                    nes_debugging_colour_palette_swatch_highlight,
                    nes_debugging_memory_visualization,
                    nes_debugging_cpu_status_visualization,
                    nes_debugging_cpu_instructions_visualization,
                )
                    .chain(),
            );
    }
}

pub struct EmulatorScreenPlugin;
impl Plugin for EmulatorScreenPlugin {
    fn build(&self, app: &mut App) {
        let (s, r) = crossbeam_channel::unbounded();

        let render_app = app
            .insert_resource(EmulatorScreenSender(s))
            .sub_app_mut(RenderApp);

        let mut graph = render_app.world_mut().resource_mut::<RenderGraph>();
        graph.add_node(EmulatorScreenCopy, EmulatorScreenCopyDriver);
        graph.add_node_edge(EmulatorScreenCopy, bevy::render::graph::CameraDriverLabel); // perform emulator screen copy before rendering the camera

        render_app.insert_resource(EmulatorScreenReceiver(r));
    }
}

#[derive(Resource, Deref)]
pub struct EmulatorScreenSender(Sender<(Buffer, Extent3d, Handle<Image>)>);

#[derive(Resource, Deref)]
struct EmulatorScreenReceiver(Receiver<(Buffer, Extent3d, Handle<Image>)>);

#[derive(Debug, Hash, PartialEq, Eq, Clone, RenderLabel)]
struct EmulatorScreenCopy;

#[derive(Default)]
struct EmulatorScreenCopyDriver;

impl render_graph::Node for EmulatorScreenCopyDriver {
    fn run(
        &self,
        _graph: &mut RenderGraphContext,
        render_context: &mut RenderContext,
        world: &World,
    ) -> Result<(), NodeRunError> {
        let receiver = world.get_resource::<EmulatorScreenReceiver>().unwrap();

        if let Ok((buffer, buffer_dimensions, target_image_handle)) = receiver.try_recv() {
            let gpu_images = world
                .get_resource::<RenderAssets<bevy::render::texture::GpuImage>>()
                .unwrap();
            let target_image = gpu_images.get(&target_image_handle).unwrap();

            let mut encoder = render_context
                .render_device()
                .create_command_encoder(&CommandEncoderDescriptor::default());

            let copy_buffer = ImageCopyBuffer {
                buffer: &buffer,
                layout: ImageDataLayout {
                    bytes_per_row: Some(buffer_dimensions.width * 4),
                    rows_per_image: None,
                    ..default()
                },
            };
            encoder.copy_buffer_to_texture(
                copy_buffer,
                target_image.texture.as_image_copy(),
                buffer_dimensions,
            );

            let render_queue = world
                .get_resource::<bevy::render::renderer::RenderQueue>()
                .unwrap();
            render_queue.submit(std::iter::once(encoder.finish()));
        }

        Ok(())
    }
}

#[derive(Component)]
struct NesEmulator {
    nes: Box<Nes>,
    screen_target: Handle<Image>,
}

#[derive(Component)]
struct NesDebugExtensions {
    selected_palette: usize,
    memory_page: u8,
    pattern_table_visualizations: [Handle<Image>; NUM_NES_PATTERN_TABLES],
    colour_palette_visualization: [Handle<Image>; NUM_NES_COLOUR_PALETTES],
}

#[derive(Component)]
struct MemoryDebugView;

#[derive(Component)]
struct CpuStatusDebugView;

#[derive(Component)]
struct ColourPaletteSwatchView(pub usize);

#[derive(Component)]
struct EmulatorStateView;

#[derive(Component)]
struct EmulatorFrameCounter;

#[derive(Default)]
enum CodeOffset {
    #[default]
    Centered,
    OffsetFromCenter(i32),
    OffsetFromTop(usize),
    OffsetFromBottom(usize),
}

#[derive(Component)]
struct CodeDebugView {
    max_visible_instructions: usize,
    active_instruction_position: CodeOffset,
}

fn default_nes_input_map() -> InputMap<NesInput> {
    let mut input_map = InputMap::default();
    input_map.insert(NesInput::A, KeyCode::KeyK);
    input_map.insert(NesInput::B, KeyCode::KeyM);
    input_map.insert(NesInput::Start, KeyCode::KeyJ);
    input_map.insert(NesInput::Select, KeyCode::KeyN);
    input_map.insert(NesInput::Dpad, VirtualDPad::wasd());
    input_map
}

fn default_nes_debugging_input_map() -> InputMap<NesDebuggingActions> {
    let mut input_map = InputMap::default();
    input_map.insert(NesDebuggingActions::RunEmulation, KeyCode::F5);
    input_map.insert(NesDebuggingActions::PauseEmulation, KeyCode::F6);
    input_map.insert(NesDebuggingActions::StepInstruction, KeyCode::F10);
    input_map.insert(NesDebuggingActions::StepFrame, KeyCode::F11);
    input_map.insert(NesDebuggingActions::PrevMemoryPage, KeyCode::ArrowLeft);
    input_map.insert(NesDebuggingActions::NextMemoryPage, KeyCode::ArrowRight);
    input_map.insert(NesDebuggingActions::PrevColourPalette, KeyCode::KeyO);
    input_map.insert(NesDebuggingActions::NextColourPalette, KeyCode::KeyP);
    input_map
}

fn setup_nes(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    asset_server: Res<AssetServer>,
    emulator_state: Res<State<EmulationState>>,
) {
    const CHOSEN_FONT_INDEX: usize = 1;
    let fonts = [
        asset_server.load("fonts/PressStart2P-vaV7.ttf"),
        asset_server.load("fonts/Emulogic-zrEw.ttf"),
    ];

    // need to create a texture asset for the NES screen
    let buffer = [0xFFu8; NES_SCREEN_WIDTH * NES_SCREEN_HEIGHT * 4];
    let nes_screen = Image::new_fill(
        Extent3d {
            width: NES_SCREEN_WIDTH as u32,
            height: NES_SCREEN_HEIGHT as u32,
            ..default()
        },
        TextureDimension::D2,
        &buffer,
        TextureFormat::bevy_default(),
        RenderAssetUsages::all(),
    );
    let nes_screen_handle = images.add(nes_screen);

    let pattern_buffer = [0xFFu8; NES_PATTERN_TABLE_WIDTH * NES_PATTERN_TABLE_HEIGHT * 4];
    let pattern_table_visualizations = (0..NUM_NES_PATTERN_TABLES)
        .into_iter()
        .map(|_| {
            images.add(Image::new_fill(
                Extent3d {
                    width: NES_PATTERN_TABLE_WIDTH as u32,
                    height: NES_PATTERN_TABLE_HEIGHT as u32,
                    ..default()
                },
                TextureDimension::D2,
                &pattern_buffer,
                TextureFormat::bevy_default(),
                RenderAssetUsages::all(),
            ))
        })
        .collect::<Vec<_>>();

    let palette_buffer = [0xFFu8; NUM_COLOURS_IN_NES_PALETTE * 4];
    let colour_palette_visualizations = (0..NUM_NES_COLOUR_PALETTES)
        .into_iter()
        .map(|_| {
            images.add(Image::new_fill(
                Extent3d {
                    width: NES_PALETTE_SWATCH_PIXEL_WIDTH as u32,
                    height: 1,
                    ..default()
                },
                TextureDimension::D2,
                &palette_buffer,
                TextureFormat::bevy_default(),
                RenderAssetUsages::all(),
            ))
        })
        .collect::<Vec<_>>();

    let mut nes = Box::new(Nes::new(&NES_TEST));
    nes.reset();

    let mut nes_emulator = commands.spawn_empty();
    nes_emulator.insert(NesEmulator {
        nes,
        screen_target: nes_screen_handle.clone(),
    });
    nes_emulator.insert(NesDebugExtensions {
        selected_palette: 0,
        memory_page: 0,
        pattern_table_visualizations: [
            pattern_table_visualizations[0].clone(),
            pattern_table_visualizations[1].clone(),
        ],
        colour_palette_visualization: [
            colour_palette_visualizations[0].clone(),
            colour_palette_visualizations[1].clone(),
            colour_palette_visualizations[2].clone(),
            colour_palette_visualizations[3].clone(),
            colour_palette_visualizations[4].clone(),
            colour_palette_visualizations[5].clone(),
            colour_palette_visualizations[6].clone(),
            colour_palette_visualizations[7].clone(),
        ],
    });
    nes_emulator.insert(InputManagerBundle::with_map(default_nes_input_map()));
    nes_emulator.insert(InputManagerBundle::with_map(default_nes_input_map()));
    nes_emulator.insert(InputManagerBundle::with_map(
        default_nes_debugging_input_map(),
    ));

    // UI setup
    commands
        .spawn(NodeBundle {
            style: Style {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                flex_direction: FlexDirection::Row,
                align_items: AlignItems::Center,
                padding: UiRect::all(Val::Px(10.0)),
                justify_content: JustifyContent::Center,
                row_gap: Val::Px(10.0),
                ..default()
            },
            ..default()
        })
        .with_children(|parent| {
            parent
                .spawn(NodeBundle {
                    style: Style {
                        width: Val::Px(NES_SCREEN_WIDTH as f32 * 2.0),
                        height: Val::Px(NES_SCREEN_WIDTH as f32 * 2.0),
                        margin: UiRect::all(Val::Px(5.0)),
                        flex_direction: FlexDirection::Row,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    ..default()
                })
                .with_children(|parent| {
                    parent
                        .spawn(NodeBundle {
                            style: Style {
                                flex_direction: FlexDirection::Column,
                                align_items: AlignItems::Center,
                                ..default()
                            },
                            ..default()
                        })
                        .with_children(|parent| {
                            parent.spawn((
                                TextBundle::from_section(
                                    format!("Emulator Status: {:?}", emulator_state.get()),
                                    TextStyle {
                                        font: fonts[CHOSEN_FONT_INDEX].clone(),
                                        font_size: 12.0,
                                        color: Color::WHITE,
                                        ..default()
                                    },
                                ),
                                EmulatorStateView,
                            ));

                            parent.spawn((
                                TextBundle::from_section(
                                    "Frame Counter: 0",
                                    TextStyle {
                                        font: fonts[CHOSEN_FONT_INDEX].clone(),
                                        font_size: 12.0,
                                        color: Color::WHITE,
                                        ..default()
                                    },
                                ),
                                EmulatorFrameCounter,
                            ));

                            parent.spawn((ImageBundle {
                                style: Style {
                                    width: Val::Px(NES_SCREEN_WIDTH as f32 * 2.0),
                                    height: Val::Px(NES_SCREEN_HEIGHT as f32 * 2.0),
                                    aspect_ratio: Some(
                                        (NES_SCREEN_WIDTH as f32) / (NES_SCREEN_HEIGHT as f32),
                                    ),
                                    ..default()
                                },
                                image: UiImage::new(nes_screen_handle.clone()),
                                ..default()
                            },));
                        });
                });

            parent
                .spawn(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    ..default()
                })
                .with_children(|parent| {
                    parent
                        .spawn(NodeBundle {
                            style: Style {
                                flex_direction: FlexDirection::Row,
                                padding: UiRect::horizontal(Val::Px(5.0)),
                                margin: UiRect::all(Val::Px(5.0)),
                                ..default()
                            },
                            ..default()
                        })
                        .with_children(|parent| {
                            parent
                                .spawn(NodeBundle {
                                    style: Style {
                                        flex_direction: FlexDirection::Column,
                                        align_items: AlignItems::FlexStart,
                                        ..default()
                                    },
                                    ..default()
                                })
                                .with_children(|parent| {
                                    let max_visible_instructions = 32;
                                    parent.spawn((
                                        TextBundle {
                                            text: Text {
                                                sections: (0..max_visible_instructions)
                                                    .map(|_| {
                                                        TextSection::new(
                                                            "Code\n",
                                                            TextStyle {
                                                                font: fonts[CHOSEN_FONT_INDEX]
                                                                    .clone(),
                                                                font_size: 12.0,
                                                                color: Color::WHITE,
                                                                ..default()
                                                            },
                                                        )
                                                    })
                                                    .collect(),
                                                justify: JustifyText::Left,
                                                linebreak_behavior: BreakLineOn::NoWrap,
                                            },
                                            style: Style {
                                                flex_grow: 0.0,
                                                flex_shrink: 0.0,
                                                min_width: Val::Px(280.0),
                                                ..default()
                                            },
                                            ..default()
                                        },
                                        CodeDebugView {
                                            max_visible_instructions,
                                            active_instruction_position: CodeOffset::OffsetFromTop(
                                                0,
                                            ),
                                        },
                                    ));
                                });

                            parent
                                .spawn(NodeBundle {
                                    style: Style {
                                        flex_direction: FlexDirection::Column,
                                        align_items: AlignItems::FlexStart,
                                        margin: UiRect::all(Val::Px(5.0)),
                                        ..default()
                                    },
                                    ..default()
                                })
                                .with_children(|parent| {
                                    parent.spawn((
                                        TextBundle {
                                            text: Text {
                                                sections: vec![
                                            TextSection::new(
                                                "CPU Status\n",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " N",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " V",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " -",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " B",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " D",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " I",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " Z",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                " C",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                            TextSection::new(
                                                "\nPC: $0000\nA: $00 \nX: $00 \nY: $00\nSP: $00",
                                                TextStyle {
                                                    font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                    font_size: 12.0,
                                                    color: Color::WHITE,
                                                    ..default()
                                                },
                                            ),
                                        ],
                                                justify: JustifyText::Center,
                                                linebreak_behavior: BreakLineOn::NoWrap,
                                            },
                                            ..default()
                                        },
                                        CpuStatusDebugView,
                                    ));

                                    parent.spawn((
                                        TextBundle {
                                            text: Text {
                                                sections: vec![TextSection::new(
                                                    "No memory fetched for visualization yet.",
                                                    TextStyle {
                                                        font: fonts[CHOSEN_FONT_INDEX].clone(),
                                                        font_size: 12.0,
                                                        color: Color::WHITE,
                                                        ..default()
                                                    },
                                                )],
                                                justify: JustifyText::Left,
                                                linebreak_behavior: BreakLineOn::NoWrap,
                                            },
                                            style: Style {
                                                align_self: AlignSelf::FlexEnd,
                                                ..default()
                                            },
                                            ..default()
                                        },
                                        MemoryDebugView,
                                    ));
                                });
                        });

                    parent
                        .spawn(NodeBundle {
                            style: Style {
                                flex_direction: FlexDirection::Row,
                                column_gap: Val::Px(5.0),
                                ..default()
                            },
                            ..default()
                        })
                        .with_children(|parent| {
                            for i in 0..NUM_NES_COLOUR_PALETTES {
                                parent
                                    .spawn((
                                        NodeBundle {
                                            style: Style {
                                                flex_direction: FlexDirection::Column,
                                                align_items: AlignItems::Center,
                                                justify_items: JustifyItems::Center,
                                                align_content: AlignContent::Center,
                                                justify_content: JustifyContent::Center,
                                                width: Val::Px(
                                                    NES_PALETTE_SWATCH_PIXEL_WIDTH as f32 + 10.0,
                                                ),
                                                height: Val::Px(16.0 + 10.0),
                                                ..default()
                                            },
                                            background_color: BackgroundColor::DEFAULT,
                                            ..default()
                                        },
                                        ColourPaletteSwatchView(i),
                                    ))
                                    .with_children(|parent| {
                                        parent.spawn(ImageBundle {
                                            style: Style {
                                                width: Val::Px(
                                                    NES_PALETTE_SWATCH_PIXEL_WIDTH as f32,
                                                ),
                                                height: Val::Px(16.0),
                                                ..default()
                                            },
                                            image: UiImage::new(
                                                colour_palette_visualizations[i].clone(),
                                            ),
                                            ..default()
                                        });
                                    });
                            }
                        });

                    parent
                        .spawn(NodeBundle {
                            style: Style {
                                margin: UiRect::all(Val::Px(5.0)),
                                flex_direction: FlexDirection::Row,
                                ..default()
                            },
                            ..default()
                        })
                        .with_children(|parent| {
                            parent.spawn(ImageBundle {
                                style: Style {
                                    width: Val::Px(NES_PATTERN_TABLE_WIDTH as f32 * 2.0),
                                    height: Val::Px(NES_PATTERN_TABLE_HEIGHT as f32 * 2.0),
                                    margin: UiRect::all(Val::Px(5.0)),
                                    ..default()
                                },
                                image: UiImage::new(pattern_table_visualizations[0].clone()),
                                ..default()
                            });

                            parent.spawn(ImageBundle {
                                style: Style {
                                    width: Val::Px(NES_PATTERN_TABLE_WIDTH as f32 * 2.0),
                                    height: Val::Px(NES_PATTERN_TABLE_HEIGHT as f32 * 2.0),
                                    margin: UiRect::all(Val::Px(5.0)),
                                    ..default()
                                },
                                image: UiImage::new(pattern_table_visualizations[1].clone()),
                                ..default()
                            });
                        });
                });
        });
}

fn nes_frame(
    mut query: Query<(&mut NesEmulator, &ActionState<NesInput>)>,
    screen_sender: ResMut<EmulatorScreenSender>,
    render_device: Res<RenderDevice>,
) {
    // need to prepare an ImageCopyBuffer during the frame, once the frame is complete
    // the buffer will be used to update the texture asset
    for (mut nes_emulator, _joypad1) in query.iter_mut() {
        let mut nes_screen_data = [0 as u8; NES_SCREEN_WIDTH * NES_SCREEN_HEIGHT * 4];
        nes_emulator.nes.frame(|x, y, r, g, b| {
            let i = (y * NES_SCREEN_WIDTH + x) * 4;
            nes_screen_data[i] = r;
            nes_screen_data[i + 1] = g;
            nes_screen_data[i + 2] = b;
            nes_screen_data[i + 3] = 0xFF;
        });

        let nes_screen_buffer = render_device.create_buffer_with_data(&BufferInitDescriptor {
            label: Some("NES Screen 0"),
            contents: &nes_screen_data,
            usage: bevy::render::render_resource::BufferUsages::COPY_SRC,
        });

        screen_sender
            .send((
                nes_screen_buffer,
                Extent3d {
                    width: NES_SCREEN_WIDTH as u32,
                    height: NES_SCREEN_HEIGHT as u32,
                    depth_or_array_layers: 1,
                },
                nes_emulator.screen_target.clone(),
            ))
            .unwrap();
    }
}

fn nes_paused(
    query: Query<&ActionState<NesDebuggingActions>>,
    mut next_state: ResMut<NextState<EmulationState>>,
) {
    for action_state in query.iter() {
        if action_state.pressed(&NesDebuggingActions::RunEmulation) {
            next_state.set(EmulationState::Running);
        }
        // else if action_state.just_pressed(&NesDebuggingActions::StepInstruction) {
        //     next_state.set(EmulationState::StepOnce);
        // }
        else if action_state.just_pressed(&NesDebuggingActions::StepFrame) {
            next_state.set(EmulationState::RunOnce);
        }
    }
}

fn nes_pause_from_run_once(mut next_state: ResMut<NextState<EmulationState>>) {
    next_state.set(EmulationState::Paused);
}

fn nes_pause_from_running(
    query: Query<&ActionState<NesDebuggingActions>>,
    mut next_state: ResMut<NextState<EmulationState>>,
) {
    for action_state in query.iter() {
        if action_state.pressed(&NesDebuggingActions::PauseEmulation) {
            next_state.set(EmulationState::Paused);
        }
    }
}

fn nes_debugging_show_emulation_state(
    mut text_target: Query<&mut Text, With<EmulatorStateView>>,
    emulator_state: Res<State<EmulationState>>,
) {
    for mut text in text_target.iter_mut() {
        text.sections[0].value = format!("Emulator Status: {:?}", emulator_state.get());
    }
}

fn nes_debugging_show_emulator_frame_counter(
    query: Query<&NesEmulator>,
    mut text_target: Query<&mut Text, With<EmulatorFrameCounter>>,
) {
    for (nes_emulator, mut text) in query.iter().zip(text_target.iter_mut()) {
        text.sections[0].value =
            format!("Frame Counter: {}", nes_emulator.nes.system_frame_counter());
    }
}

fn nes_debugging_pattern_table_visualization(
    mut query: Query<(&mut NesEmulator, &NesDebugExtensions)>,
    screen_sender: ResMut<EmulatorScreenSender>,
    render_device: Res<RenderDevice>,
) {
    for (mut nes_emulator, nes_debug_ext) in query.iter_mut() {
        for i in 0..NUM_NES_PATTERN_TABLES {
            let mut pattern_table_data =
                [0 as u8; NES_PATTERN_TABLE_WIDTH * NES_PATTERN_TABLE_HEIGHT * 4];
            nes_emulator.nes.fill_buffer_with_pattern_table(
                i,
                nes_debug_ext.selected_palette,
                |x, y, r, g, b| {
                    let i = (y * NES_PATTERN_TABLE_WIDTH + x) * 4;
                    pattern_table_data[i] = r;
                    pattern_table_data[i + 1] = g;
                    pattern_table_data[i + 2] = b;
                    pattern_table_data[i + 3] = 0xFF;
                },
            );

            let pattern_table_image =
                render_device.create_buffer_with_data(&BufferInitDescriptor {
                    label: Some(format!("NES Pattern Table {}", i).as_str()),
                    contents: &pattern_table_data,
                    usage: bevy::render::render_resource::BufferUsages::COPY_SRC,
                });

            screen_sender
                .send((
                    pattern_table_image,
                    Extent3d {
                        width: NES_PATTERN_TABLE_WIDTH as u32,
                        height: NES_PATTERN_TABLE_HEIGHT as u32,
                        depth_or_array_layers: 1,
                    },
                    nes_debug_ext.pattern_table_visualizations[i].clone(),
                ))
                .unwrap();
        }
    }
}

fn nes_debugging_colour_palette_visualization(
    mut query: Query<(&mut NesEmulator, &NesDebugExtensions)>,
    screen_sender: ResMut<EmulatorScreenSender>,
    render_device: Res<RenderDevice>,
) {
    for (mut nes_emulator, nes_debug_ext) in query.iter_mut() {
        for palette in 0..NUM_NES_COLOUR_PALETTES {
            // copy buffers need 256 bytes per row https://docs.rs/wgpu/latest/wgpu/constant.COPY_BYTES_PER_ROW_ALIGNMENT.html
            let mut colour_palette_data = [0 as u8; NES_PALETTE_SWATCH_PIXEL_WIDTH * 4];
            const INDIVIDUAL_COLOUR_PIXEL_COUNT: usize =
                NES_PALETTE_SWATCH_PIXEL_WIDTH / NUM_COLOURS_IN_NES_PALETTE;
            nes_emulator.nes.fill_buffer_with_palette_colours(
                palette as u8,
                |palette_offset, r, g, b| {
                    let i = palette_offset as usize * (INDIVIDUAL_COLOUR_PIXEL_COUNT * 4);
                    for j in 0..INDIVIDUAL_COLOUR_PIXEL_COUNT {
                        let j = j * 4;
                        colour_palette_data[j + i] = r;
                        colour_palette_data[j + i + 1] = g;
                        colour_palette_data[j + i + 2] = b;
                        colour_palette_data[j + i + 3] = 0xFF;
                    }
                },
            );

            let colour_palette_image =
                render_device.create_buffer_with_data(&BufferInitDescriptor {
                    label: Some(format!("NES Colour Palette Swatch {}", palette).as_str()),
                    contents: &colour_palette_data,
                    usage: bevy::render::render_resource::BufferUsages::COPY_SRC,
                });

            screen_sender
                .send((
                    colour_palette_image,
                    Extent3d {
                        width: 64,
                        height: 1,
                        depth_or_array_layers: 1,
                    },
                    nes_debug_ext.colour_palette_visualization[palette].clone(),
                ))
                .unwrap();
        }
    }
}

fn nes_debugging_colour_palette_swatch_highlight(
    mut nes_query: Query<(&mut NesDebugExtensions, &ActionState<NesDebuggingActions>)>,
    mut swatch_query: Query<
        (&mut BackgroundColor, &ColourPaletteSwatchView),
        With<ColourPaletteSwatchView>,
    >,
) {
    const HIGHLIGH_COLOUR: Color = Color::linear_rgb(0.0, 1.0, 0.0);
    for (mut nes_debugging, action_state) in nes_query.iter_mut() {
        match (
            action_state.just_pressed(&NesDebuggingActions::PrevColourPalette),
            action_state.just_pressed(&NesDebuggingActions::NextColourPalette),
        ) {
            (true, _) => {
                if nes_debugging.selected_palette == 0 {
                    nes_debugging.selected_palette = NUM_NES_COLOUR_PALETTES - 1;
                } else {
                    nes_debugging.selected_palette -= 1;
                }
            }
            (_, true) => {
                if nes_debugging.selected_palette == NUM_NES_COLOUR_PALETTES - 1 {
                    nes_debugging.selected_palette = 0;
                } else {
                    nes_debugging.selected_palette += 1;
                }
            }
            _ => {}
        }

        for (mut background_color, swatch) in swatch_query.iter_mut() {
            background_color.0 = if swatch.0 == nes_debugging.selected_palette {
                HIGHLIGH_COLOUR
            } else {
                Color::NONE
            };
        }
    }
}

fn nes_debugging_memory_visualization(
    mut nes_query: Query<(
        &mut NesEmulator,
        &mut NesDebugExtensions,
        &ActionState<NesDebuggingActions>,
    )>,
    mut memory_text_target: Query<&mut Text, With<MemoryDebugView>>,
) {
    for ((mut nes_emulator, mut nes_debugging, debug_action_state), mut text_target) in
        nes_query.iter_mut().zip(memory_text_target.iter_mut())
    {
        match (
            debug_action_state.just_pressed(&NesDebuggingActions::NextMemoryPage),
            debug_action_state.just_pressed(&NesDebuggingActions::PrevMemoryPage),
        ) {
            (true, _) => nes_debugging.memory_page = nes_debugging.memory_page.wrapping_add(1),
            (_, true) => nes_debugging.memory_page = nes_debugging.memory_page.wrapping_sub(1),
            _ => {}
        }

        let memory_page = (nes_debugging.memory_page as u16) << 8;
        let mut memory_text = String::new();

        let memory_cols = 16u16;
        let memory_rows = 256 / memory_cols;
        for row in 0..memory_rows {
            memory_text.push_str(format!("${:04X}: ", memory_page + row * memory_cols).as_str());
            memory_text.push_str({
                let mem = (0..memory_cols)
                    .map(|col| {
                        format!(
                            "{:02X}",
                            nes_emulator
                                .nes
                                .read_byte(memory_page + row * memory_cols + col)
                        )
                    })
                    .collect::<Vec<_>>();
                mem.join(" ").as_str()
            });
            memory_text.push('\n');
        }

        text_target.sections[0].value = memory_text.clone();
    }
}

fn nes_debugging_cpu_status_visualization(
    nes_query: Query<&NesEmulator>,
    mut cpu_status_text_target: Query<&mut Text, With<CpuStatusDebugView>>,
) {
    static GREEN: Color = Color::linear_rgb(0.0, 1.0, 0.0);
    static RED: Color = Color::linear_rgb(1.0, 0.0, 0.0);
    for (nes_emulator, mut text_target) in nes_query.iter().zip(cpu_status_text_target.iter_mut()) {
        let status = [
            nes_emulator.nes.cpu().status_sign_bit(),
            nes_emulator.nes.cpu().status_overflow_bit(),
            nes_emulator.nes.cpu().status_unused_bit(),
            nes_emulator.nes.cpu().status_break_command_bit(),
            nes_emulator.nes.cpu().status_decimal_mode_bit(),
            nes_emulator.nes.cpu().status_interrupt_disable_bit(),
            nes_emulator.nes.cpu().status_zero_bit(),
            nes_emulator.nes.cpu().status_carry_bit(),
        ];
        for i in 0..8 {
            text_target.sections[i + 1].style.color = if status[i] { GREEN } else { RED };
        }
        text_target.sections[9].value = format!(
            "\nPC: ${:04X}\nA: ${:02X} \nX: ${:02X} \nY: ${:02X}\nSP: ${:02X}",
            nes_emulator.nes.cpu().program_counter(),
            nes_emulator.nes.cpu().register_a(),
            nes_emulator.nes.cpu().register_x(),
            nes_emulator.nes.cpu().register_y(),
            nes_emulator.nes.cpu().stack_pointer()
        );
    }
}

fn nes_debugging_cpu_instructions_visualization(
    mut nes_query: Query<&mut NesEmulator>,
    mut code_text_target: Query<(&mut Text, &CodeDebugView), With<CodeDebugView>>,
) {
    static ACTIVE_COLOUR: Color = Color::linear_rgb(0.0, 1.0, 1.0);
    static INACTIVE_COLOUR: Color = Color::linear_rgb(1.0, 1.0, 1.0);
    for (mut nes_emulator, (mut text_target, code_debug_view)) in
        nes_query.iter_mut().zip(code_text_target.iter_mut())
    {
        let (active_instruction_line, num_instructions_to_disassemble) =
            match code_debug_view.active_instruction_position {
                CodeOffset::Centered => {
                    let line = code_debug_view.max_visible_instructions / 2;
                    (line, code_debug_view.max_visible_instructions - line)
                }
                CodeOffset::OffsetFromCenter(offset) => {
                    let line =
                        ((code_debug_view.max_visible_instructions / 2) as i32 + offset) as usize;
                    (line, code_debug_view.max_visible_instructions - line)
                }
                CodeOffset::OffsetFromTop(offset) => {
                    (offset, code_debug_view.max_visible_instructions - offset)
                }
                CodeOffset::OffsetFromBottom(offset) => {
                    (code_debug_view.max_visible_instructions - offset, offset)
                }
            };

        let pc = nes_emulator.nes.cpu().program_counter();
        let disassembly = nes_emulator
            .nes
            .cpu_mut()
            .disassemble_range(pc, num_instructions_to_disassemble);
        let num_blanks = num_instructions_to_disassemble - disassembly.len();

        for (i, disassembled_instruction) in disassembly.iter().enumerate() {
            let line = active_instruction_line + i;
            if line < code_debug_view.max_visible_instructions {
                text_target.sections[line].value =
                    format!("{}\n", disassembled_instruction.as_str());
                text_target.sections[line].style.color = if line == active_instruction_line {
                    ACTIVE_COLOUR
                } else {
                    INACTIVE_COLOUR
                };
            }
        }

        for i in 0..num_blanks {
            let line = active_instruction_line + disassembly.len() + i;
            if line < code_debug_view.max_visible_instructions {
                text_target.sections[line].value = "\n".to_string();
                text_target.sections[line].style.color = INACTIVE_COLOUR;
            }
        }
    }
}
