use bevy::prelude::*;
use nes_bevy_plugin::NesPlugin;

mod nes;
mod nes_bevy_plugin;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitflags;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(NesPlugin)
        .add_systems(Startup, setup_app)
        .add_systems(Update, close_on_esc)
        .run();
}

fn setup_app(mut commands: Commands) {
    commands.spawn((
        Camera2dBundle {
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 1.0)),
            ..Default::default()
        },
        IsDefaultUiCamera,
    ));
}

fn close_on_esc(
    mut commands: Commands,
    focused_windows: Query<(Entity, &Window)>,
    input: Res<ButtonInput<KeyCode>>,
) {
    for (window, focus) in focused_windows.iter() {
        if !focus.focused {
            continue;
        }

        if input.just_pressed(KeyCode::Escape) {
            commands.entity(window).despawn();
        }
    }
}
