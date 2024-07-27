use crate::nes::{
    NES_PATTERN_TABLE_HEIGHT, NES_PATTERN_TABLE_WIDTH, NES_SCREEN_HEIGHT, NES_SCREEN_WIDTH,
    NUM_NES_PATTERN_TABLES,
};

use super::nes::Nes;
use bevy::{
    prelude::*,
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
};
use crossbeam_channel::{Receiver, Sender};

static NES_TEST: &'static [u8] = include_bytes!("../assets/roms/nestest.nes");

pub struct NesPlugin;

impl Plugin for NesPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup_nes)
            .add_systems(Update, nes_frame)
            .add_plugins(EmulatorScreenPlugin);
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
    pattern_table_visualizations: [Handle<Image>; NUM_NES_PATTERN_TABLES],
}

fn setup_nes(mut commands: Commands, mut images: ResMut<Assets<Image>>) {
    // need to create a texture asset for the NES screen
    let buffer = [0xFF as u8; NES_SCREEN_WIDTH * NES_SCREEN_HEIGHT * 4];
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

    let pattern_buffer = [0xFF as u8; NES_PATTERN_TABLE_WIDTH * NES_PATTERN_TABLE_HEIGHT * 4];
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

    let mut nes = Box::new(Nes::new(&NES_TEST));
    nes.reset();

    commands
        .spawn(NodeBundle {
            style: Style {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                flex_direction: FlexDirection::Row,
                align_items: AlignItems::Center,
                padding: UiRect::all(Val::Px(10.0)),
                justify_content: JustifyContent::SpaceBetween,
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
                    parent.spawn((
                        NesEmulator {
                            nes,
                            screen_target: nes_screen_handle.clone(),
                        },
                        NesDebugExtensions {
                            pattern_table_visualizations: [
                                pattern_table_visualizations[0].clone(),
                                pattern_table_visualizations[1].clone(),
                            ],
                        },
                        ImageBundle {
                            style: Style {
                                width: Val::Px(NES_SCREEN_WIDTH as f32 * 2.0),
                                height: Val::Px(NES_SCREEN_HEIGHT as f32 * 2.0),
                                ..default()
                            },
                            image: UiImage::new(nes_screen_handle.clone()),
                            ..default()
                        },
                    ));
                });

            parent
                .spawn(NodeBundle {
                    style: Style {
                        width: Val::Px(NES_PATTERN_TABLE_WIDTH as f32 * 4.0),
                        height: Val::Px(NES_PATTERN_TABLE_HEIGHT as f32 * 2.0),
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
}

fn nes_frame(
    mut query: Query<(&mut NesEmulator, &NesDebugExtensions)>,
    screen_sender: ResMut<EmulatorScreenSender>,
    render_device: Res<RenderDevice>,
) {
    // need to prepare an ImageCopyBuffer during the frame, once the frame is complete
    // the buffer will be used to update the texture asset
    for (mut nes_emulator, nes_debug_ext) in query.iter_mut() {
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

        for i in 0..NUM_NES_PATTERN_TABLES {
            let mut pattern_table_data =
                [0 as u8; NES_PATTERN_TABLE_WIDTH * NES_PATTERN_TABLE_HEIGHT * 4];
            nes_emulator
                .nes
                .fill_buffer_with_pattern_table(i, 0, |x, y, r, g, b| {
                    let i = (y * NES_PATTERN_TABLE_WIDTH + x) * 4;
                    pattern_table_data[i] = r;
                    pattern_table_data[i + 1] = g;
                    pattern_table_data[i + 2] = b;
                    pattern_table_data[i + 3] = 0xFF;
                });

            let pattern_table_image =
                render_device.create_buffer_with_data(&BufferInitDescriptor {
                    label: Some("NES Pattern Table 0"),
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
