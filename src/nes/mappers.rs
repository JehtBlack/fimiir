use self::inesmapper003::INesMapper003;
use self::nrom::NROM;

mod inesmapper003;
mod nrom;

const INES_HEADER_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

enum NesFormat {
    INes,
    Nes2_0,
}

/// https://www.nesdev.org/wiki/INES
#[repr(C, packed)]
struct INesHeader {
    identifier: [u8; 4],
    prg_rom_chunks: u8,
    chr_rom_chunks: u8,

    /// mapper, mirroring, battery, trainer
    flags_6: u8,

    /// mapper, vs/playchoice, nes 2.0 identifier
    flags_7: u8,

    prg_ram_size: u8,
    tv_system: u8,

    /// tv system, prg ram presence
    flags_10: u8,

    zero: [u8; 5],
}

/// https://www.nesdev.org/wiki/NES_2.0
#[repr(C, packed)]
struct Nes2_0header {
    identifier: [u8; 4],
    prg_rom_chunks: u8,
    chr_rom_chunks: u8,

    /// mapper, mirroring, battery, trainer
    flags_6: u8,

    /// mapper, console type, nes 2.0 identifier
    flags_7: u8,

    /// mapper, submapper
    flags_8: u8,

    /// prg rom size (MSB), chr rom size (MSB)
    flags_9: u8,

    /// prg ram size/eeprom size,
    flags_10: u8,

    chr_ram_size: u8,

    cpu_ppu_timing_mode: u8,

    // vs ppu and hardware types
    vs_system_type: u8,

    misc_roms: u8,

    default_expansion_device: u8,
}

#[allow(dead_code)]
pub(crate) enum MirroringMode {
    Horizontal,
    Vertical,
    SingleScreenLow,
    SingleScreenHigh,
    FourScreen,
}

pub(crate) trait Mapper
where
    Self: Send + Sync,
{
    fn cpu_read_byte(&self, addr: u16) -> Option<u8>;
    fn cpu_write_byte(&mut self, addr: u16, data: u8) -> bool;
    fn ppu_read_byte(&self, addr: u16) -> Option<u8>;
    fn ppu_write_byte(&mut self, addr: u16, data: u8) -> bool;
    fn mirroring_mode(&self) -> &MirroringMode;
    #[allow(dead_code)]
    fn reset(&mut self) {}
    fn scanline(&mut self) {}
    fn irq_state(&self) -> bool {
        false
    }
    fn clear_irq(&mut self) {}
}

pub fn load_cartridge(rom_data: &[u8]) -> Box<dyn Mapper> {
    if rom_data.len() < 16 {
        panic!("Invalid ROM data");
    }

    if rom_data[..4] != INES_HEADER_TAG {
        panic!("Invalid iNES header");
    }

    let trainer = if rom_data[6] & 0x04 != 0 { 512 } else { 0 };
    let format = if rom_data[7] & 0x0C == 0x08 {
        NesFormat::Nes2_0
    } else {
        NesFormat::INes
    };

    let (mapper_id, mirroring_mode, prg_banks, prg_memory_size, chr_banks, chr_memory_size) =
        match format {
            NesFormat::INes => {
                let header = unsafe { rom_data.align_to::<INesHeader>().1.get_unchecked(0) };
                let mirroring_mode = if header.flags_6 & 0x01 != 0 {
                    MirroringMode::Vertical
                } else {
                    MirroringMode::Horizontal
                };
                let mapper_id = (header.flags_7 & 0xF0) | (header.flags_6 >> 4);
                (
                    mapper_id,
                    mirroring_mode,
                    header.prg_rom_chunks,
                    header.prg_rom_chunks as usize * 0x4000,
                    header.chr_rom_chunks,
                    header.chr_rom_chunks.max(1) as usize * 0x2000,
                )
            }
            #[allow(unreachable_code)]
            NesFormat::Nes2_0 => {
                todo!("NES 2.0 format is not fully supported yet");
                let header = unsafe { rom_data.align_to::<Nes2_0header>().1.get_unchecked(0) };
                let mapper_id = (header.flags_7 & 0xF0) | (header.flags_6 >> 4);
                let mirroring_mode = if header.flags_6 & 0x01 != 0 {
                    MirroringMode::Vertical
                } else {
                    MirroringMode::Horizontal
                };
                let prg_banks =
                    header.prg_rom_chunks as u16 | (((header.flags_9 & 0x0F) as u16) << 8);
                let chr_banks =
                    (header.chr_rom_chunks as u16 | ((header.flags_9 & 0xF0) as u16) << 4).max(1);
                (
                    mapper_id,
                    mirroring_mode,
                    prg_banks as u8,
                    prg_banks as usize * 0x4000,
                    chr_banks as u8,
                    chr_banks as usize * 0x2000,
                )
            }
        };

    let rom_data = &rom_data[(16 + trainer)..];

    let mapper: Box<dyn Mapper> = match mapper_id {
        0 => Box::new(NROM::new(
            mirroring_mode,
            prg_banks,
            prg_memory_size,
            chr_banks,
            chr_memory_size,
            rom_data,
        )),
        3 => Box::new(INesMapper003::new(
            mirroring_mode,
            prg_banks,
            prg_memory_size,
            chr_memory_size,
            rom_data,
        )),
        _ => panic!("Unsupported mapper: {}", mapper_id),
    };

    mapper
}
