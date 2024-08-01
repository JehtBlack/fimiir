use super::{mappers::Mapper, ppu::Ppu};

const RAM_START: u16 = 0x0000;
const RAM_END: u16 = 0x1FFF;

const PPU_START: u16 = 0x2000;
const PPU_END: u16 = 0x3FFF;

const APU_CHANNELS_START: u16 = 0x4000;
const APU_CHANNELS_END: u16 = 0x4013;
const APU_STATUS: u16 = 0x4015;
const APU_FRAME_COUNTER: u16 = 0x4017;

const OAM_DMA: u16 = 0x4014;

const JOYPAD_POLL: u16 = 0x4016;

const JOYPAD1: u16 = 0x4016;
const JOYPAD2: u16 = 0x4017;

pub(crate) struct Bus {
    cpu_ram: [u8; 2 * 1024],
    ppu: Ppu,
    dma_page: u8,
    dma_addr: u8,
    dma_data: u8,
    dma_transfer: bool,
    dma_dummy: bool,
}

impl Bus {
    pub fn new(mapper: Box<dyn Mapper>) -> Self {
        Bus {
            cpu_ram: [0; 2 * 1024],
            ppu: Ppu::new(mapper),
            dma_page: 0x00,
            dma_addr: 0x00,
            dma_data: 0x00,
            dma_transfer: false,
            dma_dummy: true,
        }
    }

    pub fn reset(&mut self) {
        self.ppu.reset();

        self.dma_page = 0x00;
        self.dma_addr = 0x00;
        self.dma_data = 0x00;

        self.dma_transfer = false;
        self.dma_dummy = true;
    }

    pub fn dma_transfer_active(&self) -> bool {
        self.dma_transfer
    }

    pub fn dma_transfer(&mut self, system_cycle: usize) {
        if self.dma_dummy {
            self.dma_dummy = system_cycle % 2 == 0;
        } else {
            if system_cycle % 2 == 0 {
                self.dma_data =
                    self.read_byte(((self.dma_page as u16) << 8) | self.dma_addr as u16);
            } else {
                self.ppu.write_oam(self.dma_addr, self.dma_data);
                self.dma_addr = self.dma_addr.wrapping_add(1);
                if self.dma_addr == 0x00 {
                    self.dma_transfer = false;
                    self.dma_dummy = true;
                }
            }
        }
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        match self.ppu.mapper().cpu_read_byte(addr) {
            Some(data) => data,
            None => match addr {
                RAM_START..=RAM_END => self.cpu_ram[(addr & 0x07FF) as usize],
                PPU_START..=PPU_END => self.ppu.read_byte(addr & 0x0007),
                _ => 0,
            },
        }
    }

    pub fn write_byte(&mut self, addr: u16, data: u8) {
        match self.ppu.mapper().cpu_write_byte(addr, data) {
            false => match addr {
                RAM_START..=RAM_END => self.cpu_ram[(addr & 0x07FF) as usize] = data,
                PPU_START..=PPU_END => self.ppu.write_byte(addr & 0x0007, data),
                OAM_DMA => {
                    self.dma_page = data;
                    self.dma_addr = 0x00;
                    self.dma_transfer = true;
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn fill_buffer_with_pattern_table<F>(
        &mut self,
        table_index: usize,
        colour_palette_index: usize,
        set_pixel_action: &mut F,
    ) where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        self.ppu.fill_buffer_with_pattern_table(
            table_index,
            colour_palette_index,
            set_pixel_action,
        );
    }

    pub fn fill_buffer_with_palette_colours<F>(&mut self, palette: u8, set_pixel_action: &mut F)
    where
        F: FnMut(usize, u8, u8, u8),
    {
        self.ppu
            .fill_buffer_with_palette_colours(palette, set_pixel_action);
    }

    /// returns the number of PPU cycles that have passed, if a NMI should be triggered and the IRQ state
    pub fn tick<F>(&mut self, cycle: usize, set_pixel_action: &mut F) -> (usize, bool, bool)
    where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        let cycles = self.ppu.tick(set_pixel_action);

        // APU tick every 6 PPU ticks
        // On the first ever frame of the system, the PPU should tick 3 cycles
        // (only NMI could change this and the CPU hasn't even ticked yet)
        // This means that this check would be false on the first frame and
        // the APU would be 3 ticks behind
        // To combat this we'll pretend the cycles are 3 ahead and use that to
        // determine if the APU should tick
        let apu_should_tick = (cycle + cycles + 3) % 6 == 0;

        // APU High Frequency tick every PPU tick

        let irq_state = self.ppu.mapper().irq_state();
        self.ppu.mapper().clear_irq();
        (cycles, self.ppu.nmi(), irq_state)
    }
}

#[cfg(test)]
use super::mappers::MirroringMode;

#[cfg(test)]
struct TestCartridge {
    mem: [u8; 0xFF],
}

#[cfg(test)]
impl TestCartridge {
    pub fn new() -> Self {
        TestCartridge { mem: [0; 0xFF] }
    }
}

#[cfg(test)]
impl Mapper for TestCartridge {
    fn cpu_read_byte(&self, addr: u16) -> Option<u8> {
        Some(self.mem[(addr & 0xFF) as usize])
    }

    fn cpu_write_byte(&mut self, addr: u16, data: u8) -> bool {
        self.mem[(addr & 0xFF) as usize] = data;
        true
    }

    fn ppu_read_byte(&self, _addr: u16) -> Option<u8> {
        None
    }

    fn ppu_write_byte(&mut self, _addr: u16, _data: u8) -> bool {
        false
    }

    fn mirroring_mode(&self) -> &MirroringMode {
        &MirroringMode::Horizontal
    }
}

#[cfg(test)]
impl Default for Bus {
    fn default() -> Self {
        let cart_prg = Box::new(TestCartridge::new());
        let cart_chr = Box::new(TestCartridge::new());
        Bus {
            cpu_ram: [0; 2 * 1024],
            ppu: Ppu::new(cart_chr),
            dma_page: 0x00,
            dma_addr: 0x00,
            dma_data: 0x00,
            dma_transfer: false,
            dma_dummy: true,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bus_read_byte() {
        let mut bus = Bus::default();
        bus.cpu_ram[0x0000] = 0x42;
        assert_eq!(bus.read_byte(0x0000), 0x42);
    }

    #[test]
    fn test_bus_write_byte() {
        let mut bus = Bus::default();
        bus.write_byte(0x0000, 0x42);
        assert_eq!(bus.cpu_ram[0x0000], 0x42);
    }
}
