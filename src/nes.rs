mod bus;
mod cpu;
mod mappers;
mod opcodes;
mod ppu;

pub const NES_SCREEN_WIDTH: usize = 256;
pub const NES_SCREEN_HEIGHT: usize = 240;

pub const NUM_NES_PATTERN_TABLES: usize = 2;
pub const NES_PATTERN_TABLE_WIDTH: usize = 128;
pub const NES_PATTERN_TABLE_HEIGHT: usize = 128;

pub struct Nes {
    cpu: cpu::Cpu,
    system_cycle_counter: usize,
}

impl Nes {
    pub fn new(rom_data: &[u8]) -> Self {
        let mapper = mappers::load_cartridge(rom_data);
        Nes {
            cpu: cpu::Cpu::new(mapper),
            system_cycle_counter: 0,
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.system_cycle_counter = 0;
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        self.cpu.read_byte(addr)
    }

    pub fn fill_buffer_with_pattern_table<F>(
        &mut self,
        table_index: usize,
        colour_palette_index: usize,
        mut set_pixel_action: F,
    ) where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        self.cpu.fill_buffer_with_pattern_table(
            table_index,
            colour_palette_index,
            &mut set_pixel_action,
        );
    }

    pub fn cpu(&self) -> &cpu::Cpu {
        &self.cpu
    }

    pub fn cpu_mut(&mut self) -> &mut cpu::Cpu {
        &mut self.cpu
    }

    pub fn frame<F>(&mut self, mut set_pixel_action: F)
    where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        self.system_cycle_counter = self
            .cpu
            .tick_frame(self.system_cycle_counter, &mut set_pixel_action);

        // prevent eventual overflow by resetting the system cycles every 3 frames which won't affect the internal timings (NTSC)
        // this may not work at all for PAL and others, it may break very subtly in those cases
        if self.system_cycle_counter % 3 == 0 {
            self.system_cycle_counter = 0;
        }
    }
}
