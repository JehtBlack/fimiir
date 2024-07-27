use super::{Mapper, MirroringMode};

pub(crate) struct INesMapper003 {
    prg_memory: Vec<u8>,
    chr_memory: Vec<u8>,

    prg_banks: u8,

    hardware_mirror_mode: MirroringMode,

    chr_bank_select: u8,
}

impl INesMapper003 {
    pub fn new(
        mirroring_mode: MirroringMode,
        prg_banks: u8,
        prg_memory_size: usize,
        chr_memory_size: usize,
        rom_data: &[u8],
    ) -> Self {
        INesMapper003 {
            prg_memory: Vec::from(&rom_data[..prg_memory_size]),
            chr_memory: Vec::from(&rom_data[prg_memory_size..(prg_memory_size + chr_memory_size)]),
            prg_banks,
            hardware_mirror_mode: mirroring_mode,
            chr_bank_select: 0,
        }
    }
}

impl Mapper for INesMapper003 {
    fn cpu_read_byte(&self, addr: u16) -> Option<u8> {
        match addr {
            0x8000..=0xFFFF => {
                let mapped_addr = addr & (if self.prg_banks > 1 { 0x7FFF } else { 0x3FFF });
                Some(self.prg_memory[mapped_addr as usize])
            }
            _ => None,
        }
    }

    fn cpu_write_byte(&mut self, addr: u16, data: u8) -> bool {
        match addr {
            0x8000..=0xFFFF => {
                self.chr_bank_select = data & 0x03;
                true
            }
            _ => false,
        }
    }

    fn ppu_read_byte(&self, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x1FFF => {
                let mapped_addr = (self.chr_bank_select as u16 * 0x2000) + addr;
                Some(self.chr_memory[mapped_addr as usize])
            }
            _ => None,
        }
    }

    fn ppu_write_byte(&mut self, _addr: u16, _data: u8) -> bool {
        false
    }

    fn reset(&mut self) {}

    fn mirroring_mode(&self) -> &MirroringMode {
        &self.hardware_mirror_mode
    }

    fn scanline(&mut self) {}

    fn irq_state(&self) -> bool {
        false
    }

    fn clear_irq(&mut self) {}
}
