use super::mappers::{Mapper, MirroringMode};
use super::{NES_SCREEN_HEIGHT, NES_SCREEN_WIDTH, NUM_COLOURS_IN_NES_PALETTE};

#[derive(Clone, Copy)]
struct PpuColour {
    channels: [u8; 3],
}

bitflags! {
    struct PpuFlags: u8 {
        const SPRITE_OVERFLOW       = 0b00100000;
        const SPRITE_ZERO_HIT       = 0b01000000;
        const VBLANK                = 0b10000000;
    }

    struct PpuMask: u8 {
        const GREYSCALE             = 0b00000001;
        const SHOW_BACKGROUND_LEFT  = 0b00000010;
        const SHOW_SPRITES_LEFT     = 0b00000100;
        const SHOW_BACKGROUND       = 0b00001000;
        const SHOW_SPRITES          = 0b00010000;
        const RED_EMPHASIS          = 0b00100000;
        const GREEN_EMPHASIS        = 0b01000000;
        const BLUE_EMPHASIS         = 0b10000000;
    }

}

#[allow(dead_code)]
enum PpuCtrlComponents {
    NametableX,
    NametableY,
    IncrementMode,
    PatternSprite,
    PatternBackground,
    SpriteSize,
    SlaveMode,
    EnableNmi,
}

struct PpuCtrl {
    nametable_x: u8,
    nametable_y: u8,
    increment_mode: u8,
    pattern_sprite: u8,
    pattern_background: u8,
    sprite_size: u8,
    slave_mode: u8,
    enable_nmi: u8,
}

impl PpuCtrl {
    fn new() -> Self {
        PpuCtrl {
            nametable_x: 0,
            nametable_y: 0,
            increment_mode: 0,
            pattern_sprite: 0,
            pattern_background: 0,
            sprite_size: 0,
            slave_mode: 0,
            enable_nmi: 0,
        }
    }

    fn from_u8(value: u8) -> Self {
        PpuCtrl {
            nametable_x: value & 0x01,
            nametable_y: (value >> 1) & 0x01,
            increment_mode: (value >> 2) & 0x01,
            pattern_sprite: (value >> 3) & 0x01,
            pattern_background: (value >> 4) & 0x01,
            sprite_size: (value >> 5) & 0x01,
            slave_mode: (value >> 6) & 0x01,
            enable_nmi: (value >> 7) & 0x01,
        }
    }

    fn contains(&self, component: PpuCtrlComponents) -> bool {
        match component {
            PpuCtrlComponents::NametableX => self.nametable_x == 1,
            PpuCtrlComponents::NametableY => self.nametable_y == 1,
            PpuCtrlComponents::IncrementMode => self.increment_mode == 1,
            PpuCtrlComponents::PatternSprite => self.pattern_sprite == 1,
            PpuCtrlComponents::PatternBackground => self.pattern_background == 1,
            PpuCtrlComponents::SpriteSize => self.sprite_size == 1,
            PpuCtrlComponents::SlaveMode => self.slave_mode == 1,
            PpuCtrlComponents::EnableNmi => self.enable_nmi == 1,
        }
    }
}

enum LoopyRegisterComponents {
    CoarseX,
    CoarseY,
    NametableX,
    NametableY,
    FineY,
}

#[derive(Clone, Copy)]
struct LoopyRegister {
    coarse_x: u16,
    coarse_y: u16,
    nametable_x: u16,
    nametable_y: u16,
    fine_y: u16,
}

impl LoopyRegister {
    const COARSE_MASK: u16 = 0b00000000_00011111;
    const NAMETABLE_MASK: u16 = 0b00000000_00000001;
    const FINE_MASK: u16 = 0b00000000_00000111;

    const COARSE_X_BIT_OFFSET: u16 = 0;
    const COARSE_Y_BIT_OFFSET: u16 = 5;
    const NAMETABLE_X_BIT_OFFSET: u16 = 10;
    const NAMETABLE_Y_BIT_OFFSET: u16 = 11;
    const FINE_Y_BIT_OFFSET: u16 = 12;

    fn new() -> Self {
        LoopyRegister {
            coarse_x: 0,
            coarse_y: 0,
            nametable_x: 0,
            nametable_y: 0,
            fine_y: 0,
        }
    }

    fn from_u16(value: u16) -> Self {
        LoopyRegister {
            coarse_x: (value >> Self::COARSE_X_BIT_OFFSET) & Self::COARSE_MASK,
            coarse_y: (value >> Self::COARSE_Y_BIT_OFFSET) & Self::COARSE_MASK,
            nametable_x: (value >> Self::NAMETABLE_X_BIT_OFFSET) & Self::NAMETABLE_MASK,
            nametable_y: (value >> Self::NAMETABLE_Y_BIT_OFFSET) & Self::NAMETABLE_MASK,
            fine_y: (value >> Self::FINE_Y_BIT_OFFSET) & Self::FINE_MASK,
        }
    }

    fn as_u16(&self) -> u16 {
        ((self.coarse_x & Self::COARSE_MASK) << Self::COARSE_X_BIT_OFFSET)
            | ((self.coarse_y & Self::COARSE_MASK) << Self::COARSE_Y_BIT_OFFSET)
            | ((self.nametable_x & Self::NAMETABLE_MASK) << Self::NAMETABLE_X_BIT_OFFSET)
            | ((self.nametable_y & Self::NAMETABLE_MASK) << Self::NAMETABLE_Y_BIT_OFFSET)
            | ((self.fine_y & Self::FINE_MASK) << Self::FINE_Y_BIT_OFFSET)
    }

    fn set(&mut self, component: LoopyRegisterComponents, value: u16) {
        match component {
            LoopyRegisterComponents::CoarseX => self.coarse_x = value,
            LoopyRegisterComponents::CoarseY => self.coarse_y = value,
            LoopyRegisterComponents::NametableX => self.nametable_x = value,
            LoopyRegisterComponents::NametableY => self.nametable_y = value,
            LoopyRegisterComponents::FineY => self.fine_y = value,
        }
    }
}

impl From<LoopyRegister> for u16 {
    fn from(register: LoopyRegister) -> Self {
        register.as_u16()
    }
}

#[derive(Clone, Copy)]
#[repr(C, packed)]
struct ObjectAttributeEntry {
    y: u8,
    id: u8,
    attribute: u8,
    x: u8,
}

pub(crate) struct Ppu {
    colour_palette: [PpuColour; 64],

    name_table: [u8; 2048],
    palette_table: [u8; 32],
    pattern_table: [u8; 8192],

    frame_complete: bool,
    nmi: bool,

    status: PpuFlags,
    mask: PpuMask,
    control: PpuCtrl,
    address_latch: bool,
    ppu_data_buffer: u8,

    vram_addr: LoopyRegister,
    tram_addr: LoopyRegister,
    fine_x: u8,

    bg_next_tile_id: u8,
    bg_next_tile_attrib: u8,
    bg_next_tile_lsb: u8,
    bg_next_tile_msb: u8,

    bg_shifter_pattern_lo: u16,
    bg_shifter_pattern_hi: u16,
    bg_shifter_attrib_lo: u16,
    bg_shifter_attrib_hi: u16,

    object_attribute_memory: [ObjectAttributeEntry; 64],
    oam_addr: u8,

    sprites_on_scanline: [ObjectAttributeEntry; 8],
    sprite_count_on_scanline: u8,
    sprite_shifter_pattern_lo: [u8; 8],
    sprite_shifter_pattern_hi: [u8; 8],

    sprite_zero_hit_possible: bool,
    sprite_zero_being_rendered: bool,

    scanline: i16,
    cycle: i16,

    clock_count: usize,
    num_cycles_to_complete_this_tick: usize,

    mapper: Box<dyn Mapper>,

    frame_count: usize,
}

impl Ppu {
    pub fn new(mapper: Box<dyn Mapper>) -> Self {
        let colour_palette: [PpuColour; 64] = [
            PpuColour::new(84, 84, 84),
            PpuColour::new(0, 30, 116),
            PpuColour::new(8, 16, 144),
            PpuColour::new(48, 0, 136),
            PpuColour::new(68, 0, 100),
            PpuColour::new(92, 0, 48),
            PpuColour::new(84, 4, 0),
            PpuColour::new(60, 24, 0),
            PpuColour::new(32, 42, 0),
            PpuColour::new(8, 58, 0),
            PpuColour::new(0, 64, 0),
            PpuColour::new(0, 60, 0),
            PpuColour::new(0, 50, 60),
            PpuColour::new(0, 0, 0),
            PpuColour::new(0, 0, 0),
            PpuColour::new(0, 0, 0),
            PpuColour::new(152, 150, 152),
            PpuColour::new(8, 76, 196),
            PpuColour::new(48, 50, 236),
            PpuColour::new(92, 30, 228),
            PpuColour::new(136, 20, 176),
            PpuColour::new(160, 20, 100),
            PpuColour::new(152, 34, 32),
            PpuColour::new(120, 60, 0),
            PpuColour::new(84, 90, 0),
            PpuColour::new(40, 114, 0),
            PpuColour::new(8, 124, 0),
            PpuColour::new(0, 118, 40),
            PpuColour::new(0, 102, 120),
            PpuColour::new(0, 0, 0),
            PpuColour::new(0, 0, 0),
            PpuColour::new(0, 0, 0),
            PpuColour::new(236, 238, 236),
            PpuColour::new(76, 154, 236),
            PpuColour::new(120, 124, 236),
            PpuColour::new(176, 98, 236),
            PpuColour::new(228, 84, 236),
            PpuColour::new(236, 88, 180),
            PpuColour::new(236, 106, 100),
            PpuColour::new(212, 136, 32),
            PpuColour::new(160, 170, 0),
            PpuColour::new(116, 196, 0),
            PpuColour::new(76, 208, 32),
            PpuColour::new(56, 204, 108),
            PpuColour::new(56, 180, 204),
            PpuColour::new(60, 60, 60),
            PpuColour::new(0, 0, 0),
            PpuColour::new(0, 0, 0),
            PpuColour::new(236, 238, 236),
            PpuColour::new(168, 204, 236),
            PpuColour::new(188, 188, 236),
            PpuColour::new(212, 178, 236),
            PpuColour::new(236, 174, 236),
            PpuColour::new(236, 174, 212),
            PpuColour::new(236, 180, 176),
            PpuColour::new(228, 196, 144),
            PpuColour::new(204, 210, 120),
            PpuColour::new(180, 222, 120),
            PpuColour::new(168, 226, 144),
            PpuColour::new(152, 226, 180),
            PpuColour::new(160, 214, 228),
            PpuColour::new(160, 162, 160),
            PpuColour::new(0, 0, 0),
            PpuColour::new(0, 0, 0),
        ];

        Ppu {
            colour_palette,
            name_table: [0; 2048],
            palette_table: [0; 32],
            pattern_table: [0; 8192],
            frame_complete: false,
            nmi: false,
            status: PpuFlags::empty(),
            mask: PpuMask::empty(),
            control: PpuCtrl::new(),
            address_latch: false,
            ppu_data_buffer: 0,
            vram_addr: LoopyRegister::new(),
            tram_addr: LoopyRegister::new(),
            fine_x: 0,
            bg_next_tile_id: 0,
            bg_next_tile_attrib: 0,
            bg_next_tile_lsb: 0,
            bg_next_tile_msb: 0,
            bg_shifter_pattern_lo: 0,
            bg_shifter_pattern_hi: 0,
            bg_shifter_attrib_lo: 0,
            bg_shifter_attrib_hi: 0,
            object_attribute_memory: [ObjectAttributeEntry {
                y: 0,
                id: 0,
                attribute: 0,
                x: 0,
            }; 64],
            oam_addr: 0,
            sprites_on_scanline: [ObjectAttributeEntry {
                y: 0,
                id: 0,
                attribute: 0,
                x: 0,
            }; 8],
            sprite_count_on_scanline: 0,
            sprite_shifter_pattern_lo: [0; 8],
            sprite_shifter_pattern_hi: [0; 8],
            sprite_zero_hit_possible: false,
            sprite_zero_being_rendered: false,
            scanline: 0,
            cycle: 0,
            clock_count: 0,
            num_cycles_to_complete_this_tick: 0,
            mapper,
            frame_count: 0,
        }
    }

    pub fn reset(&mut self) {
        self.fine_x = 0;
        self.address_latch = false;
        self.ppu_data_buffer = 0;
        self.scanline = -1;
        self.cycle = 0;
        self.bg_next_tile_id = 0;
        self.bg_next_tile_attrib = 0;
        self.bg_next_tile_lsb = 0;
        self.bg_next_tile_msb = 0;
        self.bg_shifter_pattern_lo = 0;
        self.bg_shifter_pattern_hi = 0;
        self.bg_shifter_attrib_lo = 0;
        self.bg_shifter_attrib_hi = 0;
        self.status = PpuFlags::empty();
        self.mask = PpuMask::empty();
        self.control = PpuCtrl::new();
        self.vram_addr = LoopyRegister::new();
        self.tram_addr = LoopyRegister::new();
        self.sprite_count_on_scanline = 0;
        self.frame_complete = false;
        self.nmi = false;
        self.oam_addr = 0;
        self.sprite_shifter_pattern_lo = [0; 8];
        self.sprite_shifter_pattern_hi = [0; 8];
        self.sprite_zero_hit_possible = false;
        self.sprite_zero_being_rendered = false;
        self.clock_count = 0;
        self.num_cycles_to_complete_this_tick = 3;
        self.frame_count = 0;
    }

    pub fn mapper(&mut self) -> &mut dyn Mapper {
        self.mapper.as_mut()
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        match addr {
            0x0002 => {
                let data = (self.status.bits() & 0xE0) | (self.ppu_data_buffer & 0x1F);
                self.status.remove(PpuFlags::VBLANK);
                self.address_latch = false;
                data
            }
            0x0004 => self.read_oam(self.oam_addr),
            0x0007 => {
                let data = self.ppu_data_buffer;
                let vram_addr = self.vram_addr.as_u16();
                self.ppu_data_buffer = self.read_vram_byte(vram_addr);

                let data = if vram_addr >= 0x3F00 {
                    self.ppu_data_buffer
                } else {
                    data
                };

                self.vram_addr = LoopyRegister::from_u16(
                    vram_addr
                        + if self.control.contains(PpuCtrlComponents::IncrementMode) {
                            32
                        } else {
                            1
                        },
                );
                data
            }
            _ => 0,
        }
    }

    pub fn write_byte(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000 => {
                self.control = PpuCtrl::from_u8(data);
                self.tram_addr.set(
                    LoopyRegisterComponents::NametableX,
                    self.control.contains(PpuCtrlComponents::NametableX) as u16,
                );
                self.tram_addr.set(
                    LoopyRegisterComponents::NametableY,
                    self.control.contains(PpuCtrlComponents::NametableY) as u16,
                );
            }
            0x0001 => {
                self.mask = PpuMask::from_bits_truncate(data);
            }
            0x0003 => {
                self.oam_addr = data;
            }
            0x0004 => self.write_oam(self.oam_addr, data),
            0x0005 => {
                if !self.address_latch {
                    self.fine_x = data & 0x07;
                    self.tram_addr
                        .set(LoopyRegisterComponents::CoarseX, (data >> 3) as u16);
                    self.address_latch = true;
                } else {
                    self.tram_addr
                        .set(LoopyRegisterComponents::FineY, (data & 0x07) as u16);
                    self.tram_addr
                        .set(LoopyRegisterComponents::CoarseY, (data >> 3) as u16);
                    self.address_latch = false;
                }
            }
            0x0006 => {
                if !self.address_latch {
                    self.tram_addr = LoopyRegister::from_u16(
                        (((data & 0x3F) as u16) << 8) | self.tram_addr.as_u16(),
                    );
                    self.address_latch = true;
                } else {
                    self.tram_addr =
                        LoopyRegister::from_u16((self.tram_addr.as_u16() & 0xFF00) | data as u16);
                    self.vram_addr = self.tram_addr;
                    self.address_latch = false;
                }
            }
            0x0007 => {
                self.write_vram_byte(self.vram_addr.as_u16(), data);
                self.vram_addr = LoopyRegister::from_u16(
                    self.vram_addr.as_u16()
                        + if self.control.contains(PpuCtrlComponents::IncrementMode) {
                            32
                        } else {
                            1
                        },
                );
            }
            _ => {}
        }
    }

    pub fn fill_buffer_with_pattern_table<F>(
        &mut self,
        table_index: usize,
        colour_palette_index: usize,
        set_pixel: &mut F,
    ) where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        let palette = (colour_palette_index & 0x07) as u8;
        for tile_y in 0..16 {
            for tile_x in 0..16 {
                let offset = (tile_y * NES_SCREEN_WIDTH + tile_x * 16) as u16;
                for row in 0..8 {
                    let i = table_index as u16;
                    let vram_addr = i * 0x1000 + offset + row;
                    let tile_lsb = self.read_vram_byte(vram_addr);
                    let tile_msb = self.read_vram_byte(vram_addr + 8);
                    for col in 0..8 {
                        let tile_lsb = (tile_lsb >> col) & 0x01;
                        let tile_msb = (tile_msb >> col) & 0x01;
                        let pixel = (tile_lsb << 1) | tile_msb;
                        let colour = self.get_colour_from_palette_ram(palette, pixel);
                        set_pixel(
                            tile_x * 8 + (7 - col),
                            tile_y * 8 + (row as usize),
                            colour.channels[0],
                            colour.channels[1],
                            colour.channels[2],
                        );
                    }
                }
            }
        }
    }

    pub fn fill_buffer_with_palette_colours<F>(&mut self, palette: u8, set_pixel: &mut F)
    where
        F: FnMut(usize, u8, u8, u8),
    {
        for palette_offset in 0..NUM_COLOURS_IN_NES_PALETTE {
            let colour = self.get_colour_from_palette_ram(palette, palette_offset as u8);
            set_pixel(
                palette_offset,
                colour.channels[0],
                colour.channels[1],
                colour.channels[2],
            );
        }
    }

    fn read_vram_byte(&self, addr: u16) -> u8 {
        let addr = addr & 0x3FFF;
        match self.mapper.ppu_read_byte(addr) {
            Some(data) => data,
            None => match addr {
                0x0000..=0x1FFF => self.pattern_table[(addr & 0x1FFF) as usize],
                0x2000..=0x3EFF => {
                    let addr = addr & 0x0FFF;
                    match self.mapper.mirroring_mode() {
                        &MirroringMode::Vertical => {
                            // Vertical Mirroring
                            // $0000..$03FF = table 0
                            // $0400..$07FF = table 1
                            // $0800..$0BFF = table 0
                            // $0C00..$0FFF = table 1
                            let name_table_section = addr & 0x0400;
                            self.name_table[(name_table_section + (addr & 0x03FF)) as usize]
                        }
                        &MirroringMode::Horizontal => {
                            // Horizontal Mirroring
                            // $0000..$03FF = table 0
                            // $0400..$07FF = table 0
                            // $0800..$0BFF = table 1
                            // $0C00..$0FFF = table 1
                            let name_table_section = (addr & 0x0800) >> 1;
                            self.name_table[(name_table_section + (addr & 0x03FF)) as usize]
                        }
                        _ => unimplemented!("Unsupported mirroring mode"),
                    }
                }
                0x3F00..=0x3FFF => {
                    let addr = addr & 0x001F;
                    // addresses 10, 14, 18, 1C all get the 1 masked out, others are not touched
                    let addr = addr & (((((addr & 0x0002) >> 1) | (addr & 0x0001)) << 4) | 0x000F);
                    self.palette_table[addr as usize]
                        & (if self.mask.contains(PpuMask::GREYSCALE) {
                            0x30
                        } else {
                            0x3F
                        })
                }
                _ => 0,
            },
        }
    }

    fn write_vram_byte(&mut self, addr: u16, data: u8) {
        let addr = addr & 0x3FFF;
        match self.mapper.ppu_write_byte(addr, data) {
            false => match addr {
                0x0000..=0x1FFF => self.pattern_table[(addr & 0x1FFF) as usize] = data,
                0x2000..=0x3EFF => {
                    let addr = addr & 0x0FFF;
                    match self.mapper.mirroring_mode() {
                        &MirroringMode::Vertical => {
                            // Vertical Mirroring
                            // $0000..$03FF = table 0
                            // $0400..$07FF = table 1
                            // $0800..$0BFF = table 0
                            // $0C00..$0FFF = table 1
                            let name_table_section = addr & 0x0400;
                            self.name_table[(name_table_section + (addr & 0x03FF)) as usize] = data;
                        }
                        &MirroringMode::Horizontal => {
                            // Horizontal Mirroring
                            // $0000..$03FF = table 0
                            // $0400..$07FF = table 0
                            // $0800..$0BFF = table 1
                            // $0C00..$0FFF = table 1
                            let name_table_section = (addr & 0x0800) >> 1;
                            self.name_table[(name_table_section + (addr & 0x03FF)) as usize] = data;
                        }
                        _ => unimplemented!("Unsupported mirroring mode"),
                    }
                }
                0x3F00..=0x3FFF => {
                    let addr = addr & 0x001F;
                    // addresses 10, 14, 18, 1C all get the 1 masked out, others are not touched
                    let addr = addr & (((((addr & 0x0002) >> 1) | (addr & 0x0001)) << 4) | 0x000F);
                    self.palette_table[addr as usize] = data;
                }
                _ => {}
            },
            _ => {}
        }
    }

    #[allow(dead_code)]
    pub fn frame_complete(&mut self) -> bool {
        let frame_complete = self.frame_complete;
        self.frame_complete = false;
        frame_complete
    }

    pub fn nmi(&mut self) -> bool {
        let nmi = self.nmi;
        self.nmi = false;
        nmi
    }

    pub fn write_oam(&mut self, addr: u8, data: u8) {
        let oam_addr = (addr & 0xFC) >> 2;
        match addr & 0x03 {
            0 => self.object_attribute_memory[oam_addr as usize].y = data,
            1 => self.object_attribute_memory[oam_addr as usize].id = data,
            2 => self.object_attribute_memory[oam_addr as usize].attribute = data,
            3 => self.object_attribute_memory[oam_addr as usize].x = data,
            _ => unreachable!(),
        }
    }

    fn read_oam(&self, addr: u8) -> u8 {
        let oam_addr = (addr & 0xFC) >> 2;
        match addr & 0x03 {
            0 => self.object_attribute_memory[oam_addr as usize].y,
            1 => self.object_attribute_memory[oam_addr as usize].id,
            2 => self.object_attribute_memory[oam_addr as usize].attribute,
            3 => self.object_attribute_memory[oam_addr as usize].x,
            _ => unreachable!(),
        }
    }

    fn load_background_shift_registers(&mut self) {
        self.bg_shifter_pattern_lo =
            (self.bg_shifter_pattern_lo & 0xFF00) | (self.bg_next_tile_lsb as u16);
        self.bg_shifter_pattern_hi =
            (self.bg_shifter_pattern_hi & 0xFF00) | (self.bg_next_tile_msb as u16);

        self.bg_shifter_attrib_lo = (self.bg_shifter_attrib_lo & 0xFF00)
            | (if self.bg_next_tile_attrib & 0b01 != 0 {
                0xFF
            } else {
                0x00
            });
        self.bg_shifter_attrib_hi = (self.bg_shifter_attrib_hi & 0xFF00)
            | (if self.bg_next_tile_attrib & 0b10 != 0 {
                0xFF
            } else {
                0x00
            });
    }

    fn get_colour_from_palette_ram(&self, palette: u8, pixel: u8) -> PpuColour {
        let colour_selection =
            self.read_vram_byte(0x3F00 + ((palette << 2) as u16) + (pixel as u16));
        self.colour_palette[(colour_selection & 0x3F) as usize]
    }

    pub fn tick<F>(&mut self, set_pixel: &mut F) -> usize
    where
        F: FnMut(usize, usize, u8, u8, u8),
    {
        let mut cycle_in_tick = 0;

        // TODO: convert to a look up table and use rust iterators?
        while cycle_in_tick < self.num_cycles_to_complete_this_tick {
            // special behaviour that occurs on specific scanlines
            // also contains special behaviour that happens on specific cycles (pixels)
            match self.scanline {
                -1..=239 => {
                    match self.cycle {
                        0 if self.scanline == 0 => {
                            self.cycle = 1;
                        }
                        1 if self.scanline == -1 => {
                            self.status.remove(PpuFlags::VBLANK);
                            self.status.remove(PpuFlags::SPRITE_ZERO_HIT);
                            self.status.remove(PpuFlags::SPRITE_OVERFLOW);
                            self.sprite_shifter_pattern_lo = [0; 8];
                            self.sprite_shifter_pattern_hi = [0; 8];
                        }
                        2..=257 | 321..=337 => {
                            // update shift register
                            if self.mask.contains(PpuMask::SHOW_BACKGROUND) {
                                self.bg_shifter_pattern_lo <<= 1;
                                self.bg_shifter_pattern_hi <<= 1;
                                self.bg_shifter_attrib_lo <<= 1;
                                self.bg_shifter_attrib_hi <<= 1;
                            }

                            if self.mask.contains(PpuMask::SHOW_SPRITES)
                                && self.cycle >= 1
                                && self.cycle < 258
                            {
                                for i in 0..self.sprite_count_on_scanline {
                                    if self.sprites_on_scanline[i as usize].x > 0 {
                                        self.sprites_on_scanline[i as usize].x -= 1;
                                    } else {
                                        self.sprite_shifter_pattern_lo[i as usize] <<= 1;
                                        self.sprite_shifter_pattern_hi[i as usize] <<= 1;
                                    }
                                }
                            }

                            match (self.cycle - 1) % 8 {
                                0 => {
                                    self.load_background_shift_registers();
                                    self.bg_next_tile_id = self.read_vram_byte(
                                        0x2000 | (self.vram_addr.as_u16() & 0x0FFF),
                                    );
                                }
                                2 => {
                                    self.bg_next_tile_attrib = self.read_vram_byte(
                                        0x23C0
                                            | (self.vram_addr.nametable_y << 11)
                                            | (self.vram_addr.nametable_x << 10)
                                            | ((self.vram_addr.coarse_y & 0x001C) << 1)
                                            | (self.vram_addr.coarse_x >> 2),
                                    );

                                    if self.vram_addr.coarse_y & 0x02 != 0 {
                                        self.bg_next_tile_attrib >>= 4;
                                    }

                                    if self.vram_addr.coarse_x & 0x02 != 0 {
                                        self.bg_next_tile_attrib >>= 2;
                                    }

                                    self.bg_next_tile_attrib &= 0x03;
                                }
                                4 => {
                                    // load BG pattern table lo
                                    // move pattern background bit of control register to bit 13 of the address
                                    // add the next tile id (shifted 4 bits left) to the address and finally
                                    // add the fine X scroll value
                                    let bg_pattern_table_addr = (self
                                        .control
                                        .contains(PpuCtrlComponents::PatternBackground)
                                        as u16)
                                        << 12;
                                    let bg_pattern_table_addr = bg_pattern_table_addr
                                        + ((self.bg_next_tile_id as u16) << 4)
                                        + self.fine_x as u16;
                                    self.bg_next_tile_lsb =
                                        self.read_vram_byte(bg_pattern_table_addr);
                                }
                                6 => {
                                    // load BG pattern table hi
                                    // move pattern background bit of control register to bit 13 of the address
                                    // add the next tile id (shifted 4 bits left) to the address, add the fine X scroll value
                                    // and finally add an offset for the high byte
                                    let bg_pattern_table_addr = (self
                                        .control
                                        .contains(PpuCtrlComponents::PatternBackground)
                                        as u16)
                                        << 12;
                                    let bg_pattern_table_addr = bg_pattern_table_addr
                                        + ((self.bg_next_tile_id as u16) << 4)
                                        + self.fine_x as u16
                                        + 8;
                                    self.bg_next_tile_msb =
                                        self.read_vram_byte(bg_pattern_table_addr);
                                }
                                7 => {
                                    // increment scroll x
                                    if self.mask.contains(PpuMask::SHOW_BACKGROUND)
                                        || self.mask.contains(PpuMask::SHOW_SPRITES)
                                    {
                                        // reset coarse x to 0 as incrementing would cause it to overflow
                                        // and switch horizontal nametable
                                        if self.vram_addr.coarse_x == 31 {
                                            self.vram_addr.coarse_x = 0;
                                            self.vram_addr.nametable_x =
                                                !self.vram_addr.nametable_x;
                                        } else {
                                            self.vram_addr.coarse_x += 1;
                                        }
                                    }
                                }
                                _ => {}
                            }

                            if self.cycle == 256 {
                                // increment scroll y
                                if self.mask.contains(PpuMask::SHOW_BACKGROUND)
                                    || self.mask.contains(PpuMask::SHOW_SPRITES)
                                {
                                    if self.vram_addr.fine_y < 7 {
                                        self.vram_addr.fine_y += 1;
                                    } else {
                                        self.vram_addr.fine_y = 0;

                                        if self.vram_addr.coarse_y == 29 {
                                            // the coarse y standard behaviour overflows at 29 causing a wrap around
                                            // and name table switch
                                            self.vram_addr.coarse_y = 0;
                                            self.vram_addr.nametable_y =
                                                !self.vram_addr.nametable_y;
                                        } else if self.vram_addr.coarse_y == 31 {
                                            // looks pointless but the coarse y in the tram register can be written
                                            // to by the CPU and then transferred to vram register so we have to handle
                                            // the possibility of a "true" overflow
                                            self.vram_addr.coarse_y = 0;
                                        } else {
                                            self.vram_addr.coarse_y += 1;
                                        }
                                    }
                                }
                            } else if self.cycle == 257 {
                                self.load_background_shift_registers();
                                // transfer x tram addresses to vram addresses
                                if self.mask.contains(PpuMask::SHOW_BACKGROUND)
                                    || self.mask.contains(PpuMask::SHOW_SPRITES)
                                {
                                    self.vram_addr.nametable_x = self.tram_addr.nametable_x;
                                    self.vram_addr.coarse_x = self.tram_addr.coarse_x;
                                }

                                // foreground
                                if self.scanline >= 0 {
                                    self.sprites_on_scanline = [ObjectAttributeEntry::default(); 8];
                                    self.sprite_count_on_scanline = 0;

                                    let mut oam_entry = 0;
                                    self.sprite_zero_hit_possible = false;
                                    while oam_entry < 64 && self.sprite_count_on_scanline < 9 {
                                        let diff = self.scanline as i16
                                            - self.object_attribute_memory[oam_entry].y as i16;
                                        let sprite_size =
                                            if self.control.contains(PpuCtrlComponents::SpriteSize)
                                            {
                                                16
                                            } else {
                                                8
                                            };
                                        if diff >= 0
                                            && diff < sprite_size
                                            && self.sprite_count_on_scanline < 8
                                        {
                                            if oam_entry == 0 {
                                                self.sprite_zero_hit_possible = true;
                                            }

                                            self.sprites_on_scanline
                                                [self.sprite_count_on_scanline as usize] =
                                                self.object_attribute_memory[oam_entry];
                                            self.sprite_count_on_scanline += 1;
                                        }
                                        oam_entry += 1;
                                    }

                                    self.status.set(
                                        PpuFlags::SPRITE_OVERFLOW,
                                        self.sprite_count_on_scanline > 8,
                                    );
                                }
                            }
                        }
                        280..=304 if self.scanline == -1 => {
                            // transfer y tram addresses to vram addresses
                            if self.mask.contains(PpuMask::SHOW_BACKGROUND)
                                || self.mask.contains(PpuMask::SHOW_SPRITES)
                            {
                                self.vram_addr.nametable_y = self.tram_addr.nametable_y;
                                self.vram_addr.coarse_y = self.tram_addr.coarse_y;
                                self.vram_addr.fine_y = self.tram_addr.fine_y;
                            }
                        }
                        338 | 340 => {
                            self.bg_next_tile_id =
                                self.read_vram_byte(0x2000 | (self.vram_addr.as_u16() & 0x0FFF));

                            if self.cycle == 340 {
                                for i in 0..(self.sprite_count_on_scanline as usize) {
                                    let sprite_pattern_addr = if !self
                                        .control
                                        .contains(PpuCtrlComponents::SpriteSize)
                                    {
                                        // 8x8 mode
                                        let partial_addr = ((self
                                            .control
                                            .contains(PpuCtrlComponents::PatternSprite)
                                            as u16)
                                            << 12)
                                            | ((self.sprites_on_scanline[i].id as u16) << 4);
                                        let sprite_position =
                                            self.scanline - self.sprites_on_scanline[i].y as i16;
                                        let sprite_position =
                                            if self.sprites_on_scanline[i].attribute & 0x80 == 0 {
                                                sprite_position as u16
                                            } else {
                                                // flipped vertically
                                                (7 - sprite_position) as u16
                                            };
                                        partial_addr | sprite_position
                                    } else {
                                        let partial_addr =
                                            ((self.sprites_on_scanline[i].id & 0x01) as u16) << 12;
                                        let sprite_id_part =
                                            (self.sprites_on_scanline[i].id & 0xFE) as u16;
                                        let sprite_position_part = (self.scanline
                                            - self.sprites_on_scanline[i].y as i16)
                                            as u16;
                                        // 8x16 mode
                                        let (sprite_id_part, sprite_position_part) =
                                            if self.sprites_on_scanline[i].attribute & 0x80 == 0 {
                                                if (self.scanline
                                                    - self.sprites_on_scanline[i].y as i16)
                                                    < 8
                                                {
                                                    // top half of sprite
                                                    (sprite_id_part, sprite_position_part)
                                                } else {
                                                    // bottom half of sprite
                                                    (sprite_id_part + 1, sprite_position_part)
                                                }
                                            } else {
                                                // flipped vertically
                                                if (self.scanline
                                                    - self.sprites_on_scanline[i].y as i16)
                                                    < 8
                                                {
                                                    // top half of sprite

                                                    (sprite_id_part + 1, 7 - sprite_position_part)
                                                } else {
                                                    // bottom half of sprite
                                                    (sprite_id_part, 7 - sprite_position_part)
                                                }
                                            };

                                        partial_addr
                                            | (sprite_id_part << 4)
                                            | (sprite_position_part & 0x07)
                                    };

                                    let sprite_pattern_bits_lo =
                                        self.read_vram_byte(sprite_pattern_addr);
                                    let sprite_pattern_bits_hi =
                                        self.read_vram_byte(sprite_pattern_addr + 8);

                                    let (sprite_pattern_bits_lo, sprite_pattern_bits_hi) =
                                        if self.sprites_on_scanline[i].attribute & 0x40 != 0 {
                                            // flip horizontally
                                            (
                                                sprite_pattern_bits_lo.reverse_bits(),
                                                sprite_pattern_bits_hi.reverse_bits(),
                                            )
                                        } else {
                                            (sprite_pattern_bits_lo, sprite_pattern_bits_hi)
                                        };

                                    self.sprite_shifter_pattern_lo[i] = sprite_pattern_bits_lo;
                                    self.sprite_shifter_pattern_hi[i] = sprite_pattern_bits_hi;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                240 => {} // post-render scanline, no operation
                241 if self.cycle == 1 => {
                    self.status.insert(PpuFlags::VBLANK);
                    if self.control.contains(PpuCtrlComponents::EnableNmi) {
                        self.nmi = true;
                        self.num_cycles_to_complete_this_tick = 0; // exit early for system to check for NMI
                    }
                }
                _ => {}
            }

            // common cycle behaviour

            let (bg_pixel, bg_palette) = if self.mask.contains(PpuMask::SHOW_BACKGROUND) {
                let bit_mux: u16 = 0x8000 >> self.fine_x;
                let plane_0_pixel = ((self.bg_shifter_pattern_lo & bit_mux) != 0) as u8;
                let plane_1_pixel = ((self.bg_shifter_pattern_hi & bit_mux) != 0) as u8;
                let plane_0_palette = ((self.bg_shifter_attrib_lo & bit_mux) != 0) as u8;
                let plane_1_palette = ((self.bg_shifter_attrib_hi & bit_mux) != 0) as u8;

                (
                    (plane_1_pixel << 1) | plane_0_pixel,
                    (plane_1_palette << 1) | plane_0_palette,
                )
            } else {
                (0x00, 0x00)
            };

            let (fg_pixel, fg_palette, fg_priority) = if self.mask.contains(PpuMask::SHOW_SPRITES) {
                self.sprite_zero_being_rendered = false;
                let mut fg_pixel = 0x00;
                let mut fg_palette = 0x00;
                let mut fg_priority = false;
                for i in 0..(self.sprite_count_on_scanline as usize) {
                    if self.sprites_on_scanline[i].x == 0 {
                        fg_pixel = ((self.sprite_shifter_pattern_hi[i] & 0x80) >> 6)
                            | ((self.sprite_shifter_pattern_lo[i] & 0x80) >> 7);
                        fg_palette = (self.sprites_on_scanline[i].attribute & 0x03) + 0x04;
                        fg_priority = (self.sprites_on_scanline[i].attribute & 0x20) == 0;

                        if fg_pixel != 0 {
                            if i == 0 {
                                self.sprite_zero_being_rendered = true;
                            }
                            break;
                        }
                    }
                }
                (fg_pixel, fg_palette, fg_priority)
            } else {
                (0x00, 0x00, false)
            };

            let (pixel, palette) = match (bg_pixel, fg_pixel) {
                (0, 0) => (0x00, 0x00),
                (0, _) => (fg_pixel, fg_palette),
                (_, 0) => (bg_pixel, bg_palette),
                _ => {
                    // sprite zero hit detection
                    // the hit must be possible, determined earlier
                    // sprite 0 must be being rendered, determined earlier
                    // the background and foreground must both be enabled as the collision would be between the two
                    if self.sprite_zero_hit_possible
                        && self.sprite_zero_being_rendered
                        && self.mask.contains(PpuMask::SHOW_BACKGROUND)
                        && self.mask.contains(PpuMask::SHOW_SPRITES)
                    {
                        // check if rendering to the left of the screen is enabled (either background or foreground)
                        let cycle_lower_bound = if self.mask.contains(PpuMask::SHOW_BACKGROUND_LEFT)
                            || self.mask.contains(PpuMask::SHOW_SPRITES_LEFT)
                        {
                            1
                        } else {
                            9
                        };

                        // check the sprite is being rendered within the valid range of the screen
                        if self.cycle >= cycle_lower_bound && self.cycle < 258 {
                            // hit!
                            self.status.insert(PpuFlags::SPRITE_ZERO_HIT);
                        }
                    }

                    // both background and foreground are not transparent
                    if fg_priority {
                        // foreground has priority over background
                        (fg_pixel, fg_palette)
                    } else {
                        (bg_pixel, bg_palette)
                    }
                }
            };

            {
                let cycle_as_x = self.cycle - 1;
                let (x, y) = (cycle_as_x as usize, self.scanline as usize);
                if cycle_as_x >= 0
                    && x < NES_SCREEN_WIDTH
                    && self.scanline >= 0
                    && y < NES_SCREEN_HEIGHT
                {
                    let colour = self.get_colour_from_palette_ram(palette, pixel);
                    // let colour = if y == (self.frame_count % NES_SCREEN_HEIGHT) {
                    //     PpuColour::new(0xFF, 0x00, 0xFF)
                    // } else {
                    //     PpuColour::new(0x00, 0x00, 0x00)
                    // };
                    set_pixel(
                        x,
                        y,
                        colour.channels[0],
                        colour.channels[1],
                        colour.channels[2],
                    );
                }
            }

            self.cycle += 1;
            if self.mask.contains(PpuMask::SHOW_BACKGROUND)
                || self.mask.contains(PpuMask::SHOW_SPRITES)
            {
                if self.cycle == 260 && self.scanline < 240 {
                    self.mapper.scanline();
                }
            }

            // check if we need to move to the next scanline
            if self.cycle >= 341 {
                self.cycle = 0;
                self.scanline += 1;

                // check if we need to move to the next frame
                if self.scanline >= 261 {
                    self.scanline = -1;
                    self.frame_complete = true;
                    self.num_cycles_to_complete_this_tick = 0;
                    self.frame_count = self.frame_count.wrapping_add(1);
                }
            }

            cycle_in_tick += 1;
            self.clock_count += 1;
        }

        if self.nmi && cycle_in_tick < 3 {
            self.num_cycles_to_complete_this_tick = 3 - cycle_in_tick;
        } else {
            self.num_cycles_to_complete_this_tick = 3;
        }

        cycle_in_tick
    }
}

impl Default for ObjectAttributeEntry {
    fn default() -> Self {
        ObjectAttributeEntry {
            y: 0,
            id: 0,
            attribute: 0,
            x: 0,
        }
    }
}

impl PpuColour {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        PpuColour {
            channels: [r, g, b],
        }
    }
}

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
        Some(self.mem[addr as usize])
    }

    fn cpu_write_byte(&mut self, addr: u16, data: u8) -> bool {
        self.mem[addr as usize] = data;
        true
    }

    fn ppu_read_byte(&self, addr: u16) -> Option<u8> {
        Some(self.mem[addr as usize])
    }

    fn ppu_write_byte(&mut self, addr: u16, data: u8) -> bool {
        self.mem[addr as usize] = data;
        true
    }

    fn mirroring_mode(&self) -> &MirroringMode {
        &MirroringMode::Horizontal
    }

    fn scanline(&mut self) {}
}

#[cfg(test)]
impl Default for Ppu {
    fn default() -> Self {
        Ppu::new(Box::new(TestCartridge::new()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn create_ppu() {
        let _ppu = Ppu::default();
    }
}
