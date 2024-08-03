use crate::nes::{
    NES_PATTERN_TABLE_HEIGHT, NES_PATTERN_TABLE_WIDTH, NES_SCREEN_HEIGHT, NES_SCREEN_WIDTH,
    NUM_COLOURS_IN_NES_PALETTE, NUM_NES_COLOUR_PALETTES,
};

use super::nes::Nes;
use bevy::{prelude::*, render::render_asset::RenderAssetUsages, text::BreakLineOn};
use image::DynamicImage;
use leafwing_input_manager::prelude::*;

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
#[allow(dead_code)]
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
            .insert_resource(Time::<Fixed>::from_hz(60.0))
            .insert_state(EmulationState::default())
            .add_systems(Startup, setup_nes)
            .add_systems(
                Update,
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

#[derive(Component)]
struct NesEmulator {
    nes: Box<Nes>,
}

#[derive(Component)]
struct NesDebugExtensions {
    selected_palette: usize,
    memory_page: u8,
}

#[derive(Component)]
struct NesScreenViewer;

#[derive(Component)]
struct MemoryDebugViewer;

#[derive(Component)]
struct CpuStatusDebugViewer;

#[derive(Component, Deref)]
struct NesPatternTableViewer(pub usize);

#[derive(Component, Deref)]
struct NesColourPaletteSwatchViewer(pub usize);

#[derive(Component)]
struct EmulatorStateView;

#[derive(Component)]
struct EmulatorFrameCounter;

#[derive(Default)]
#[allow(dead_code)]
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

fn add_nes_screen_emulator_info(
    parent: &mut ChildBuilder,
    text_style: &TextStyle,
    emulator_state: &Res<State<EmulationState>>,
) {
    parent.spawn((
        TextBundle::from_section(
            format!("Emulator Status: {:?}", emulator_state.get()),
            text_style.clone(),
        ),
        EmulatorStateView,
    ));

    parent.spawn((
        TextBundle::from_section("Frame Counter: 0", text_style.clone()),
        EmulatorFrameCounter,
    ));
}

fn add_nes_screen(parent: &mut ChildBuilder, nes_screen_handle: Handle<Image>) {
    parent.spawn((
        ImageBundle {
            style: Style {
                width: Val::Px(NES_SCREEN_WIDTH as f32 * 2.0),
                height: Val::Px(NES_SCREEN_HEIGHT as f32 * 2.0),
                aspect_ratio: Some((NES_SCREEN_WIDTH as f32) / (NES_SCREEN_HEIGHT as f32)),
                ..default()
            },
            image: UiImage::new(nes_screen_handle.clone()),
            ..default()
        },
        NesScreenViewer,
    ));
}

fn add_debugging_ui(
    parent: &mut ChildBuilder,
    text_style: &TextStyle,
    pattern_table_visualizations: &[Handle<Image>],
    colour_palette_visualizations: &[Handle<Image>],
) {
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
                                            .map(|_| TextSection::new("Code\n", text_style.clone()))
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
                                    active_instruction_position: CodeOffset::OffsetFromTop(0),
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
                                            TextSection::new("CPU Status\n", text_style.clone()),
                                            TextSection::new(" N", text_style.clone()),
                                            TextSection::new(" V", text_style.clone()),
                                            TextSection::new(" -", text_style.clone()),
                                            TextSection::new(" B", text_style.clone()),
                                            TextSection::new(" D", text_style.clone()),
                                            TextSection::new(" I", text_style.clone()),
                                            TextSection::new(" Z", text_style.clone()),
                                            TextSection::new(" C", text_style.clone()),
                                            TextSection::new(
                                                "\nPC: $0000\nA: $00 \nX: $00 \nY: $00\nSP: $00",
                                                text_style.clone(),
                                            ),
                                        ],
                                        justify: JustifyText::Center,
                                        linebreak_behavior: BreakLineOn::NoWrap,
                                    },
                                    ..default()
                                },
                                CpuStatusDebugViewer,
                            ));

                            parent.spawn((
                                TextBundle {
                                    text: Text {
                                        sections: vec![TextSection::new(
                                            "No memory fetched for visualization yet.",
                                            text_style.clone(),
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
                                MemoryDebugViewer,
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
                    for (i, swatch) in colour_palette_visualizations.iter().enumerate() {
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
                                NesColourPaletteSwatchViewer(i),
                            ))
                            .with_children(|parent| {
                                parent.spawn((
                                    ImageBundle {
                                        style: Style {
                                            width: Val::Px(NES_PALETTE_SWATCH_PIXEL_WIDTH as f32),
                                            height: Val::Px(16.0),
                                            ..default()
                                        },
                                        image: UiImage::new(swatch.clone()),
                                        ..default()
                                    },
                                    NesColourPaletteSwatchViewer(i),
                                ));
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
                    for (i, pattern_table) in pattern_table_visualizations.iter().enumerate() {
                        parent.spawn((
                            ImageBundle {
                                style: Style {
                                    width: Val::Px(NES_PATTERN_TABLE_WIDTH as f32 * 2.0),
                                    height: Val::Px(NES_PATTERN_TABLE_HEIGHT as f32 * 2.0),
                                    margin: UiRect::all(Val::Px(5.0)),
                                    ..default()
                                },
                                image: UiImage::new(pattern_table.clone()),
                                ..default()
                            },
                            NesPatternTableViewer(i),
                        ));
                    }
                });
        });
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

    let mut nes = Box::new(Nes::new(&NES_TEST));
    nes.reset();

    let nes_screen_canvas_handle = images.add(Image::from_dynamic(
        DynamicImage::new_rgba8(NES_SCREEN_WIDTH as u32, NES_PATTERN_TABLE_HEIGHT as u32),
        true,
        RenderAssetUsages::default(),
    ));

    let pattern_table_visualization_canvas_handles = [
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(
                NES_PATTERN_TABLE_WIDTH as u32,
                NES_PATTERN_TABLE_HEIGHT as u32,
            ),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(
                NES_PATTERN_TABLE_WIDTH as u32,
                NES_PATTERN_TABLE_HEIGHT as u32,
            ),
            true,
            RenderAssetUsages::default(),
        )),
    ];

    let colour_palette_visualization_canvas_handles = [
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
        images.add(Image::from_dynamic(
            DynamicImage::new_rgba8(NES_PALETTE_SWATCH_PIXEL_WIDTH as u32, 1),
            true,
            RenderAssetUsages::default(),
        )),
    ];

    let mut nes_emulator = commands.spawn_empty();
    nes_emulator.insert(NesEmulator { nes });
    nes_emulator.insert(NesDebugExtensions {
        selected_palette: 0,
        memory_page: 0,
    });
    nes_emulator.insert(InputManagerBundle::with_map(default_nes_input_map()));
    nes_emulator.insert(InputManagerBundle::with_map(default_nes_input_map()));
    nes_emulator.insert(InputManagerBundle::with_map(
        default_nes_debugging_input_map(),
    ));

    let text_style = TextStyle {
        font: fonts[CHOSEN_FONT_INDEX].clone(),
        font_size: 12.0,
        color: Color::WHITE,
        ..default()
    };

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
                        flex_direction: FlexDirection::Column,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    ..default()
                })
                .with_children(|parent| {
                    add_nes_screen_emulator_info(parent, &text_style, &emulator_state);
                    add_nes_screen(parent, nes_screen_canvas_handle);
                });

            add_debugging_ui(
                parent,
                &text_style,
                &pattern_table_visualization_canvas_handles,
                &colour_palette_visualization_canvas_handles,
            );
        });
}

fn nes_frame(
    mut query: Query<(&mut NesEmulator, &ActionState<NesInput>)>,
    mut viewer_query: Query<&mut UiImage, With<NesScreenViewer>>,
    mut images: ResMut<Assets<Image>>,
) {
    // need to prepare an ImageCopyBuffer during the frame, once the frame is complete
    // the buffer will be used to update the texture asset
    for ((mut nes_emulator, _joypad1), mut image_target) in
        query.iter_mut().zip(viewer_query.iter_mut())
    {
        let mut nes_screen_data = vec![0; NES_SCREEN_WIDTH * NES_SCREEN_HEIGHT * 4];
        nes_emulator.nes.frame(|x, y, r, g, b| {
            let i = (y * NES_SCREEN_WIDTH + x) * 4;
            nes_screen_data[i] = r;
            nes_screen_data[i + 1] = g;
            nes_screen_data[i + 2] = b;
            nes_screen_data[i + 3] = 0xFF;
        });

        let rgba_data = image::RgbaImage::from_raw(
            NES_SCREEN_WIDTH as u32,
            NES_SCREEN_HEIGHT as u32,
            nes_screen_data,
        )
        .unwrap();
        let new_canvas = Image::from_dynamic(
            DynamicImage::ImageRgba8(rgba_data),
            true,
            RenderAssetUsages::default(),
        );
        image_target.texture = images.add(new_canvas);
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
    mut viewer_query: Query<(&mut UiImage, &NesPatternTableViewer), With<NesPatternTableViewer>>,
    mut images: ResMut<Assets<Image>>,
) {
    for (mut nes_emulator, nes_debug_ext) in query.iter_mut() {
        for (mut image_target, pattern_table) in viewer_query.iter_mut() {
            let i = pattern_table.0;
            let mut pattern_table_data =
                vec![0; NES_PATTERN_TABLE_WIDTH * NES_PATTERN_TABLE_HEIGHT * 4];
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

            let rgba_data = image::RgbaImage::from_raw(
                NES_PATTERN_TABLE_WIDTH as u32,
                NES_PATTERN_TABLE_HEIGHT as u32,
                pattern_table_data,
            )
            .unwrap();
            let new_canvas = Image::from_dynamic(
                DynamicImage::ImageRgba8(rgba_data),
                true,
                RenderAssetUsages::default(),
            );
            image_target.texture = images.add(new_canvas);
        }
    }
}

fn nes_debugging_colour_palette_visualization(
    mut query: Query<&mut NesEmulator>,
    mut viewer_query: Query<
        (&mut UiImage, &NesColourPaletteSwatchViewer),
        With<NesColourPaletteSwatchViewer>,
    >,
    mut images: ResMut<Assets<Image>>,
) {
    for mut nes_emulator in query.iter_mut() {
        for (mut image_target, swatch_id) in viewer_query.iter_mut() {
            let palette = swatch_id.0;
            // copy buffers need 256 bytes per row https://docs.rs/wgpu/latest/wgpu/constant.COPY_BYTES_PER_ROW_ALIGNMENT.html
            let mut colour_palette_data = vec![0; NES_PALETTE_SWATCH_PIXEL_WIDTH * 4];
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

            let rgba_data = image::RgbaImage::from_raw(
                NES_PALETTE_SWATCH_PIXEL_WIDTH as u32,
                1,
                colour_palette_data,
            )
            .unwrap();
            let new_canvas = Image::from_dynamic(
                DynamicImage::ImageRgba8(rgba_data),
                true,
                RenderAssetUsages::default(),
            );
            image_target.texture = images.add(new_canvas);
        }
    }
}

fn nes_debugging_colour_palette_swatch_highlight(
    mut nes_query: Query<(&mut NesDebugExtensions, &ActionState<NesDebuggingActions>)>,
    mut swatch_query: Query<
        (&mut BackgroundColor, &NesColourPaletteSwatchViewer),
        With<NesColourPaletteSwatchViewer>,
    >,
) {
    const HIGHLIGHT_COLOUR: Color = Color::linear_rgb(0.0, 1.0, 0.0);
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
                HIGHLIGHT_COLOUR
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
    mut memory_text_target: Query<&mut Text, With<MemoryDebugViewer>>,
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
    mut cpu_status_text_target: Query<&mut Text, With<CpuStatusDebugViewer>>,
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
