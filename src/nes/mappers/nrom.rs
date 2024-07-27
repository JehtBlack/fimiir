use super::{Mapper, MirroringMode};

pub(crate) struct NROM {
    prg_memory: Vec<u8>,
    chr_memory: Vec<u8>,

    prg_banks: u8,
    chr_banks: u8,

    hardware_mirror_mode: MirroringMode,
}

impl NROM {
    pub fn new(
        mirroring_mode: MirroringMode,
        prg_banks: u8,
        prg_memory_size: usize,
        chr_banks: u8,
        chr_memory_size: usize,
        rom_data: &[u8],
    ) -> Self {
        NROM {
            prg_memory: Vec::from(&rom_data[..prg_memory_size]),
            chr_memory: Vec::from(&rom_data[prg_memory_size..(prg_memory_size + chr_memory_size)]),
            prg_banks,
            chr_banks,
            hardware_mirror_mode: mirroring_mode,
        }
    }
}

impl Mapper for NROM {
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
                let mapped_addr = addr & (if self.prg_banks > 1 { 0x7FFF } else { 0x3FFF });
                self.prg_memory[mapped_addr as usize] = data;
                true
            }
            _ => false,
        }
    }

    fn ppu_read_byte(&self, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x1FFF => Some(self.chr_memory[addr as usize]),
            _ => None,
        }
    }

    fn ppu_write_byte(&mut self, addr: u16, data: u8) -> bool {
        match addr {
            0x0000..=0x1FFF => {
                if self.chr_banks == 0 {
                    self.chr_memory[addr as usize] = data;
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn mirroring_mode(&self) -> &MirroringMode {
        &self.hardware_mirror_mode
    }
}
