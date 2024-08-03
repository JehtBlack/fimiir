use super::mappers::{Mapper, MirroringMode};
use super::{
    NES_SCREEN_HEIGHT, NES_SCREEN_WIDTH, NUM_COLOURS_IN_NES_PALETTE, NUM_NES_COLOUR_PALETTES,
};

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

    struct PpuCtrl: u8 {
        const NAMETABLE_X           = 0b00000001;
        const NAMETABLE_Y           = 0b00000010;
        const INCREMENT_MODE        = 0b00000100;
        const PATTERN_SPRITE        = 0b00001000;
        const PATTERN_BACKGROUND    = 0b00010000;
        const SPRITE_SIZE           = 0b00100000;
        const SLAVE_MODE            = 0b01000000;
        const ENABLE_NMI            = 0b10000000;
    }

    struct LoopyRegister: u16 {
        const COARSE_X      = 0b00000000_00011111;
        const COARSE_Y      = 0b00000011_11100000;
        const NAMETABLE_X   = 0b00000100_00000000;
        const NAMETABLE_Y   = 0b00001000_00000000;
        const FINE_Y        = 0b01110000_00000000;
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
    address_latch: u8,
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
            control: PpuCtrl::empty(),
            address_latch: 0,
            ppu_data_buffer: 0,
            vram_addr: LoopyRegister::empty(),
            tram_addr: LoopyRegister::empty(),
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
        self.address_latch = 0;
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
        self.control = PpuCtrl::empty();
        self.vram_addr = LoopyRegister::empty();
        self.tram_addr = LoopyRegister::empty();
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
                self.address_latch = 0;
                data
            }
            0x0004 => self.read_oam_unsafe(self.oam_addr),
            0x0007 => {
                let data = self.ppu_data_buffer;
                self.ppu_data_buffer = self.read_vram_byte(self.vram_addr.bits());
                let old_vram_addr = self.vram_addr.bits();
                self.vram_addr = LoopyRegister::from_bits_truncate(
                    self.vram_addr.bits()
                        + (if self.control.contains(PpuCtrl::INCREMENT_MODE) {
                            32
                        } else {
                            1
                        }),
                );
                if old_vram_addr >= 0x3F00 {
                    self.ppu_data_buffer
                } else {
                    data
                }
            }
            _ => 0,
        }
    }

    pub fn write_byte(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000 => {
                self.control = PpuCtrl::from_bits_truncate(data);
                self.tram_addr.set(
                    LoopyRegister::NAMETABLE_X,
                    self.control.contains(PpuCtrl::NAMETABLE_X),
                );
                self.tram_addr.set(
                    LoopyRegister::NAMETABLE_Y,
                    self.control.contains(PpuCtrl::NAMETABLE_Y),
                );
            }
            0x0001 => {
                self.mask = PpuMask::from_bits_truncate(data);
            }
            0x0003 => {
                self.oam_addr = data;
            }
            0x0004 => self.write_oam_unsafe(self.oam_addr, data),
            0x0005 => {
                if self.address_latch == 0 {
                    self.fine_x = data & 0x07;

                    // Coarse X
                    self.tram_addr.insert(LoopyRegister::from_bits_truncate(
                        ((data >> 3) as u16) & LoopyRegister::COARSE_X.bits(),
                    ));
                    self.address_latch = 1;
                } else {
                    self.tram_addr
                        .set(LoopyRegister::FINE_Y, (data & 0x07) != 0);

                    // Coarse Y
                    self.tram_addr.insert(LoopyRegister::from_bits_truncate(
                        ((data as u16) << 2) & LoopyRegister::COARSE_Y.bits(),
                    ));
                    self.address_latch = 0;
                }
            }
            0x0006 => {
                if self.address_latch == 0 {
                    self.tram_addr = LoopyRegister::from_bits_truncate(
                        (((data & 0x3F) as u16) << 8) | (self.tram_addr.bits() & 0x00FF),
                    );
                    self.address_latch = 1;
                } else {
                    self.tram_addr = LoopyRegister::from_bits_truncate(
                        (self.tram_addr.bits() & 0xFF00) | (data as u16),
                    );
                    self.vram_addr = LoopyRegister::from_bits_truncate(self.tram_addr.bits());
                    self.address_latch = 0;
                }
            }
            0x0007 => {
                self.write_vram_byte(self.vram_addr.bits(), data);
                self.vram_addr = LoopyRegister::from_bits_truncate(
                    self.vram_addr.bits()
                        + if self.control.contains(PpuCtrl::INCREMENT_MODE) {
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

    pub fn write_oam_unsafe(&mut self, addr: u8, data: u8) {
        unsafe {
            *(self.object_attribute_memory.as_mut_ptr() as *mut u8).offset(addr as isize) = data;
        }
    }

    fn read_oam_unsafe(&self, addr: u8) -> u8 {
        unsafe { *(self.object_attribute_memory.as_ptr() as *const u8).offset(addr as isize) }
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
                                    self.bg_next_tile_id = self
                                        .read_vram_byte(0x2000 | (self.vram_addr.bits() & 0x0FFF));
                                }
                                2 => {
                                    // move nametable Y value to bit 11
                                    let nametable_y_bit = (self.vram_addr.bits()
                                        & LoopyRegister::NAMETABLE_Y.bits())
                                        << 10;
                                    // move nametable X value to bit 10
                                    let nametable_x_bit = (self.vram_addr.bits()
                                        & LoopyRegister::NAMETABLE_X.bits())
                                        << 10;
                                    // mask out lower two bits of coarse Y and shift right 3 bits to combine with processed coarse X
                                    let coarse_y_bits = ((self.vram_addr.bits()
                                        & LoopyRegister::COARSE_Y.bits())
                                        & 0x01C0)
                                        >> 3;
                                    // mask off the lower 2 bits and ready for combining with shifted and masked coarse Y
                                    let coarse_x_bits = (self.vram_addr.bits()
                                        & LoopyRegister::COARSE_X.bits())
                                        >> 2;
                                    self.bg_next_tile_attrib = self.read_vram_byte(
                                        0x23C0
                                            | nametable_y_bit
                                            | nametable_x_bit
                                            | coarse_y_bits
                                            | coarse_x_bits,
                                    );

                                    // check bit 1 of coarse Y
                                    if self.vram_addr.bits() & 0x0040 != 0 {
                                        self.bg_next_tile_attrib >>= 4;
                                    }

                                    // check bit 1 of coarse X
                                    if self.vram_addr.bits() & 0x0002 != 0 {
                                        self.bg_next_tile_attrib >>= 2;
                                    }

                                    self.bg_next_tile_attrib &= 0x03;
                                }
                                4 => {
                                    // load BG pattern table lo
                                    // move pattern background bit of control register to bit 13 of the address
                                    // add the next tile id (shifted 4 bits left) to the address and finally
                                    // add the fine X scroll value
                                    let bg_pattern_table_addr = ((self.control.bits()
                                        & PpuCtrl::PATTERN_BACKGROUND.bits())
                                        as u16)
                                        << 8;
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
                                    let bg_pattern_table_addr = ((self.control.bits()
                                        & PpuCtrl::PATTERN_BACKGROUND.bits())
                                        as u16)
                                        << 8;
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
                                        if (self.vram_addr.bits() & LoopyRegister::COARSE_X.bits())
                                            == 31
                                        {
                                            // reset coarse x to 0 as incrementing would cause it to overflow
                                            // and switch horizontal nametable
                                            self.vram_addr = LoopyRegister::from_bits_truncate(
                                                self.vram_addr.bits()
                                                    & !LoopyRegister::COARSE_X.bits(),
                                            );
                                            self.vram_addr.toggle(LoopyRegister::NAMETABLE_X);
                                        } else {
                                            // increment coarse x by 1
                                            let coarse_x = (self.vram_addr.bits()
                                                & LoopyRegister::COARSE_X.bits())
                                                + 1;
                                            self.vram_addr = LoopyRegister::from_bits_truncate(
                                                (self.vram_addr.bits()
                                                    & !LoopyRegister::COARSE_X.bits())
                                                    | coarse_x,
                                            );
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
                                    if (self.vram_addr.bits() & LoopyRegister::FINE_Y.bits())
                                        == 0x7000
                                    {
                                        // reset fine y to 0 as incrementing would cause it to overflow
                                        self.vram_addr = LoopyRegister::from_bits_truncate(
                                            self.vram_addr.bits() & !LoopyRegister::FINE_Y.bits(),
                                        );
                                        let mut coarse_y = (self.vram_addr.bits()
                                            & LoopyRegister::COARSE_Y.bits())
                                            >> 5;
                                        if coarse_y == 29 {
                                            // the coarse y standard behaviour overflows at 29 causing a wrap around
                                            // and name table switch
                                            coarse_y = 0;
                                            self.vram_addr.toggle(LoopyRegister::NAMETABLE_Y);
                                        } else if coarse_y == 31 {
                                            // looks pointless but the coarse y in the tram register can be written
                                            // to by the CPU and then transferred to vram register so we have to handle
                                            // the possibility of a "true" overflow
                                            coarse_y = 0;
                                        } else {
                                            coarse_y += 1;
                                        }
                                        self.vram_addr = LoopyRegister::from_bits_truncate(
                                            (self.vram_addr.bits()
                                                & !LoopyRegister::COARSE_Y.bits())
                                                | (coarse_y << 5),
                                        );
                                    } else {
                                        // increment fine y by 1
                                        let fine_y = (self.vram_addr.bits()
                                            & LoopyRegister::FINE_Y.bits())
                                            + 0x1000;
                                        self.vram_addr = LoopyRegister::from_bits_truncate(
                                            (self.vram_addr.bits() & !LoopyRegister::FINE_Y.bits())
                                                | fine_y,
                                        );
                                    }
                                }
                            } else if self.cycle == 257 {
                                // transfer x tram addresses to vram addresses
                                if self.mask.contains(PpuMask::SHOW_BACKGROUND)
                                    || self.mask.contains(PpuMask::SHOW_SPRITES)
                                {
                                    self.vram_addr = LoopyRegister::from_bits_truncate(
                                        (self.vram_addr.bits() & !LoopyRegister::COARSE_X.bits())
                                            | (self.tram_addr.bits()
                                                & LoopyRegister::COARSE_X.bits()),
                                    );
                                    self.vram_addr.set(
                                        LoopyRegister::NAMETABLE_X,
                                        self.tram_addr.contains(LoopyRegister::NAMETABLE_X),
                                    );
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
                                            if self.control.contains(PpuCtrl::SPRITE_SIZE) {
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
                                self.vram_addr = LoopyRegister::from_bits_truncate(
                                    (self.vram_addr.bits()
                                        & !(LoopyRegister::COARSE_Y.bits()
                                            | LoopyRegister::FINE_Y.bits()))
                                        | (self.tram_addr.bits() & LoopyRegister::COARSE_Y.bits())
                                        | (self.tram_addr.bits() & LoopyRegister::FINE_Y.bits()),
                                );
                                self.vram_addr.set(
                                    LoopyRegister::NAMETABLE_Y,
                                    self.tram_addr.contains(LoopyRegister::NAMETABLE_Y),
                                );
                            }
                        }
                        338 | 340 => {
                            self.bg_next_tile_id =
                                self.read_vram_byte(0x2000 | (self.vram_addr.bits() & 0x0FFF));

                            if self.cycle == 340 {
                                for i in 0..(self.sprite_count_on_scanline as usize) {
                                    let sprite_pattern_addr = if !self
                                        .control
                                        .contains(PpuCtrl::SPRITE_SIZE)
                                    {
                                        // 8x8 mode
                                        let partial_addr = (((self.control.bits()
                                            & PpuCtrl::PATTERN_SPRITE.bits())
                                            as u16)
                                            << 9)
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
                                        let sprite_position_part = ((self.scanline
                                            - self.sprites_on_scanline[i].y as i16)
                                            as u16)
                                            & 0x07;
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

                                        partial_addr | (sprite_id_part << 4) | sprite_position_part
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
                    if self.control.contains(PpuCtrl::ENABLE_NMI) {
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

            let (pixel, palette) = if bg_pixel == 0 && fg_pixel == 0 {
                // both background and foreground are transparent
                (0x00, 0x00)
            } else if bg_pixel == 0 && fg_pixel != 0 {
                // background is transparent, foreground is not
                (fg_pixel, fg_palette)
            } else if bg_pixel != 0 && fg_pixel == 0 {
                // background is not transparent, foreground is
                (bg_pixel, bg_palette)
            } else {
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

trait BitwiseReverse {
    fn reverse_bits(self) -> Self;
}

impl BitwiseReverse for u8 {
    fn reverse_bits(mut self) -> Self {
        self = (self & 0xF0) >> 4 | (self & 0x0F) << 4;
        self = (self & 0xCC) >> 2 | (self & 0x33) << 2;
        self = (self & 0xAA) >> 1 | (self & 0x55) << 1;
        self
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
