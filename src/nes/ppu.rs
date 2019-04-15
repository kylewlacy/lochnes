use std::u8;
use std::cell::Cell;
use std::ops::Generator;
use bitflags::bitflags;
use crate::nes::Nes;
use crate::video::{Video, Point, Color};

#[derive(Clone)]
pub struct Ppu {
    pub ctrl: Cell<PpuCtrlFlags>,
    pub mask: Cell<PpuMaskFlags>,
    pub status: Cell<PpuStatusFlags>,
    pub oam_addr: Cell<u8>,
    pub scroll: Cell<u16>,
    pub addr: Cell<u16>,

    // Latch used for writing to PPUSCROLL and PPUADDR (toggles after a write
    // to each, used to determine if the high bit or low bit is being written).
    pub scroll_addr_latch: Cell<bool>,

    pub nametables: Cell<[u8; 4 * 0x0400]>,
    pub oam: Cell<[u8; 0x0100]>,
    pub palette_ram: Cell<[u8; 0x20]>,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            ctrl: Cell::new(PpuCtrlFlags::from_bits_truncate(0x00)),
            mask: Cell::new(PpuMaskFlags::from_bits_truncate(0x00)),
            status: Cell::new(PpuStatusFlags::from_bits_truncate(0x00)),
            oam_addr: Cell::new(0x00),
            scroll: Cell::new(0x0000),
            addr: Cell::new(0x0000),
            scroll_addr_latch: Cell::new(false),
            nametables: Cell::new([0; 4 * 0x0400]),
            oam: Cell::new([0; 0x0100]),
            palette_ram: Cell::new([0; 0x20]),
        }
    }

    fn nametables(&self) -> &[Cell<u8>] {
        let nametables: &Cell<[u8]> = &self.nametables;
        nametables.as_slice_of_cells()
    }

    fn oam(&self) -> &[Cell<u8>] {
        let oam: &Cell<[u8]> = &self.oam;
        oam.as_slice_of_cells()
    }

    fn palette_ram(&self) -> &[Cell<u8>] {
        let palette_ram: &Cell<[u8]> = &self.palette_ram;
        palette_ram.as_slice_of_cells()
    }

    pub fn set_ppuctrl(&self, value: u8) {
        self.ctrl.set(PpuCtrlFlags::from_bits_truncate(value));
    }

    pub fn set_ppumask(&self, value: u8) {
        self.mask.set(PpuMaskFlags::from_bits_truncate(value));
    }

    fn read_addr(&self, addr: u16) -> u8 {
        let nametables = self.nametables();
        let palette_ram = self.palette_ram();

        match addr {
            0x2000..=0x2FFF => {
                let offset = addr as usize - 0x2000;
                nametables[offset].get()
            }
            0x3F00..=0x3F1F => {
                let offset = addr as usize - 0x3F00;
                palette_ram[offset].get()
            }
            _ => {
                unimplemented!("Unimplemented read from VRAM address ${:04X}", addr)
            }
        }
    }

    pub fn write_addr(&self, addr: u16, value: u8) {
        let nametables = self.nametables();
        let palette_ram = self.palette_ram();

        match addr {
            0x0000..=0x0FFF => {
                unimplemented!();
            }
            0x2000..=0x2FFF => {
                let offset = addr as usize - 0x2000;
                nametables[offset].set(value);
            }
            0x3F00..=0x3F1F => {
                let offset = addr as usize - 0x3F00;
                palette_ram[offset].set(value);
            }
            _ => {
                unimplemented!("Unimplemented write to VRAM address ${:04X}", addr)
            }
        }
    }

    pub fn write_oamaddr(&self, value: u8) {
        self.oam_addr.set(value);
    }

    pub fn write_oamdata(&self, value: u8) {
        let oam_addr = self.oam_addr.get();
        let oam = self.oam();

        oam[oam_addr as usize].set(value);

        let next_oam_addr = oam_addr.wrapping_add(1) as usize % oam.len();
        self.oam_addr.set(next_oam_addr as u8);
    }

    pub fn write_ppuscroll(&self, value: u8) {
        let latch = self.scroll_addr_latch.get();

        if latch {
            let scroll_lo = self.scroll.get() & 0x00FF;
            let scroll_hi = (value as u16) << 8;
            self.scroll.set(scroll_lo | scroll_hi);
        }
        else {
            let scroll_lo = value as u16;
            let scroll_hi = self.scroll.get() & 0xFF00;
            self.scroll.set(scroll_lo | scroll_hi);
        }

        self.scroll_addr_latch.set(!latch);
    }

    pub fn write_ppuaddr(&self, value: u8) {
        let latch = self.scroll_addr_latch.get();

        if latch {
            let addr_lo = value as u16;
            let addr_hi = self.addr.get() & 0xFF00;
            self.addr.set(addr_lo | addr_hi);
        }
        else {
            let addr_lo = self.addr.get() & 0x00FF;
            let addr_hi = (value as u16) << 8;
            self.addr.set(addr_lo | addr_hi);
        }

        self.scroll_addr_latch.set(!latch);
    }

    pub fn read_ppudata(&self) -> u8 {
        let addr = self.addr.get();
        let ctrl = self.ctrl.get();
        let stride =
            // Add 1 to the PPU address if the I flag is clear, add 32 if
            // it is set
            match ctrl.contains(PpuCtrlFlags::VRAM_ADDR_INCREMENT) {
                false => 1,
                true => 32
            };

        let value = self.read_addr(addr);
        self.addr.update(|addr| addr.wrapping_add(stride));

        value
    }

    pub fn write_ppudata(&self, value: u8) {
        let addr = self.addr.get();
        let ctrl = self.ctrl.get();
        let stride =
            // Add 1 to the PPU address if the I flag is clear, add 32 if
            // it is set
            match ctrl.contains(PpuCtrlFlags::VRAM_ADDR_INCREMENT) {
                false => 1,
                true => 32
            };

        self.write_addr(addr, value);
        self.addr.update(|addr| addr.wrapping_add(stride));
    }

    pub fn ppustatus(&self) -> u8 {
        self.status.get().bits()
    }

    pub fn run<'a>(nes: &'a Nes, video: &'a mut impl Video)
        -> impl Generator<Yield = PpuStep, Return = !> + 'a
    {
        move || {
            // TODO: Disable registers on startup until enough cycles
            // have elapsed
            for frame in 0_u64.. {
                let is_even_frame = frame % 2 == 0;
                for scanline in 0_u16..262 {
                    let cycles: u16 = match (scanline, is_even_frame) {
                        // All scanlines render in 341 cycles, except for the
                        // first scanline during odd frames
                        (0, false) => 340,
                        _ => 341,
                    };

                    for cycle in 0..cycles {
                        if scanline == 240 && cycle == 1 {
                            let _ = nes.ppu.status.update(|mut status| {
                                status.set(PpuStatusFlags::VBLANK_STARTED, true);
                                status
                            });
                            let ctrl = nes.ppu.ctrl.get();
                            if ctrl.contains(PpuCtrlFlags::VBLANK_INTERRUPT) {
                                // TODO: Generate NMI immediately if
                                // VBLANK_INTERRUPT is toggled during vblank
                                nes.cpu.nmi.set(true);
                            }
                            video.present();
                            yield PpuStep::Vblank;
                        }
                        else if scanline == 0 && cycle == 1 {
                            let _ = nes.ppu.status.update(|mut status| {
                                status.set(PpuStatusFlags::VBLANK_STARTED, false);
                                status
                            });
                            video.clear();
                        }

                        if 1 <= scanline && scanline < 241 && cycle < 256 {
                            let x = cycle;
                            let y = scanline - 1;

                            let nametables = nes.ppu.nametables();
                            let palette_ram = nes.ppu.palette_ram();
                            let palette_ram_indices = [
                                [0x00, 0x01, 0x02, 0x03],
                                [0x00, 0x05, 0x06, 0x07],
                                [0x00, 0x09, 0x0A, 0x0B],
                                [0x00, 0x0D, 0x0E, 0x0F],
                                [0x00, 0x11, 0x12, 0x13],
                                [0x00, 0x15, 0x16, 0x17],
                                [0x00, 0x19, 0x1A, 0x1B],
                                [0x00, 0x1D, 0x1E, 0x1F],
                            ];
                            let palettes = [
                                [
                                    palette_ram[palette_ram_indices[0][0]].get(),
                                    palette_ram[palette_ram_indices[0][1]].get(),
                                    palette_ram[palette_ram_indices[0][2]].get(),
                                    palette_ram[palette_ram_indices[0][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[1][0]].get(),
                                    palette_ram[palette_ram_indices[1][1]].get(),
                                    palette_ram[palette_ram_indices[1][2]].get(),
                                    palette_ram[palette_ram_indices[1][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[2][0]].get(),
                                    palette_ram[palette_ram_indices[2][1]].get(),
                                    palette_ram[palette_ram_indices[2][2]].get(),
                                    palette_ram[palette_ram_indices[2][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[3][0]].get(),
                                    palette_ram[palette_ram_indices[3][1]].get(),
                                    palette_ram[palette_ram_indices[3][2]].get(),
                                    palette_ram[palette_ram_indices[3][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[4][0]].get(),
                                    palette_ram[palette_ram_indices[4][1]].get(),
                                    palette_ram[palette_ram_indices[4][2]].get(),
                                    palette_ram[palette_ram_indices[4][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[5][0]].get(),
                                    palette_ram[palette_ram_indices[5][1]].get(),
                                    palette_ram[palette_ram_indices[5][2]].get(),
                                    palette_ram[palette_ram_indices[5][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[6][0]].get(),
                                    palette_ram[palette_ram_indices[6][1]].get(),
                                    palette_ram[palette_ram_indices[6][2]].get(),
                                    palette_ram[palette_ram_indices[6][3]].get(),
                                ],
                                [
                                    palette_ram[palette_ram_indices[7][0]].get(),
                                    palette_ram[palette_ram_indices[7][1]].get(),
                                    palette_ram[palette_ram_indices[7][2]].get(),
                                    palette_ram[palette_ram_indices[7][3]].get(),
                                ],
                            ];
                            let pattern_tables = &nes.mapper.rom.chr_rom;
                            let ppu_ctrl = nes.ppu.ctrl.get();

                            let bg_color_code = {
                                let tile_x = x / 8;
                                let tile_y = y / 8;

                                let tile_x_pixel = x % 8;
                                let tile_y_pixel = y % 8;

                                let attr_x = x / 32;
                                let attr_y = y / 32;
                                let attr_is_left = ((x / 16) % 2) == 0;
                                let attr_is_top = ((y / 16) % 2) == 0;

                                let nametable = &nametables[0x000..0x400];
                                let tile_index = (tile_y * 32 + tile_x) as usize;
                                let tile = nametable[tile_index].get();

                                let attr_index = (attr_y * 8 + attr_x) as usize;
                                let attr = nametable[0x3C0 + attr_index].get();
                                let palette_index = match (attr_is_top, attr_is_left) {
                                    (true, true)   =>  attr & 0b_0000_0011,
                                    (true, false)  => (attr & 0b_0000_1100) >> 2,
                                    (false, true)  => (attr & 0b_0011_0000) >> 4,
                                    (false, false) => (attr & 0b_1100_0000) >> 6
                                };
                                let palette = palettes[palette_index as usize];

                                let pattern_bitmask = 0b_1000_0000 >> tile_x_pixel;
                                let pattern_table_offset =
                                    if ppu_ctrl.contains(PpuCtrlFlags::BACKGROUND_PATTERN_TABLE_ADDR) {
                                        0x1000
                                    }
                                    else {
                                        0x0000
                                    };
                                let pattern_offset = pattern_table_offset + tile as usize * 16;
                                let pattern = &pattern_tables[pattern_offset..pattern_offset + 16];
                                let pattern_lo_byte = pattern[tile_y_pixel as usize];
                                let pattern_hi_byte = pattern[tile_y_pixel as usize + 8];
                                let pattern_lo_bit = (pattern_lo_byte & pattern_bitmask) != 0;
                                let pattern_hi_bit = (pattern_hi_byte & pattern_bitmask) != 0;

                                let palette_index = match (pattern_hi_bit, pattern_lo_bit) {
                                    (false, false) => 0,
                                    (false, true) => 1,
                                    (true, false) => 2,
                                    (true, true) => 3,
                                };
                                let color_code = palette[palette_index];

                                color_code
                            };

                            let sprite_index_with_color_code = {
                                let oam = nes.ppu.oam();
                                let mut objects = oam.chunks(4);
                                let object = objects.find(|object| {
                                    let object_y = object[0].get() as u16;
                                    let object_x = object[3].get() as u16;

                                    // Check if (x, y - 1) is in the bounding
                                    // box of {x=object_x, y=object_y, w=8, h=8}
                                    object_x <= x
                                        && x < object_x + 8
                                        && y >= 1
                                        && object_y <= y - 1
                                        && y - 1 < object_y + 8
                                });

                                object.map(|object| {
                                    let object_y = object[0].get() as u16;
                                    let tile_index = object[1].get();
                                    let attrs = object[2].get();
                                    let object_x = object[3].get() as u16;

                                    let flip_horizontal = (attrs & 0b_0100_0000) != 0;
                                    let flip_vertical = (attrs & 0b_1000_0000) != 0;

                                    let object_x_pixel = x - object_x;
                                    let object_y_pixel = y - 1 - object_y;

                                    let object_x_pixel =
                                        if flip_horizontal {
                                            7 - object_x_pixel
                                        }
                                        else {
                                            object_x_pixel
                                        };
                                    let object_y_pixel =
                                        if flip_vertical {
                                            7 - object_y_pixel
                                        }
                                        else {
                                            object_y_pixel
                                        };

                                    let pattern_bitmask = 0b_1000_0000 >> object_x_pixel;
                                    let pattern_table_offset =
                                        if ppu_ctrl.contains(PpuCtrlFlags::SPRITE_PATTERN_TABLE_ADDR) {
                                            0x1000
                                        }
                                        else {
                                            0x0000
                                        };
                                    let pattern_offset = pattern_table_offset + tile_index as usize * 16;
                                    let pattern = &pattern_tables[pattern_offset..pattern_offset + 16];
                                    let pattern_lo_byte = pattern[object_y_pixel as usize];
                                    let pattern_hi_byte = pattern[object_y_pixel as usize + 8];
                                    let pattern_lo_bit = (pattern_lo_byte & pattern_bitmask) != 0;
                                    let pattern_hi_bit = (pattern_hi_byte & pattern_bitmask) != 0;

                                    let palette_lo_bit = (attrs & 0b_0000_0001) != 0;
                                    let palette_hi_bit = (attrs & 0b_0000_0010) != 0;

                                    let palette_index = match (palette_hi_bit, palette_lo_bit) {
                                        (false, false) => 4,
                                        (false, true) => 5,
                                        (true, false) => 6,
                                        (true, true) => 7,
                                    };
                                    let palette = palettes[palette_index as usize];

                                    let pattern_index = match (pattern_hi_bit, pattern_lo_bit) {
                                        (false, false) => 0,
                                        (false, true) => 1,
                                        (true, false) => 2,
                                        (true, true) => 3,
                                    };

                                    let color_code = palette[pattern_index];

                                    (pattern_index, color_code)
                                })
                            };

                            let color_code = match sprite_index_with_color_code {
                                None | Some((0, _)) => bg_color_code,
                                Some((_, sprite_color_code)) => sprite_color_code,
                            };
                            let color = nes_color_code_to_rgb(color_code);
                            let point = Point { x, y };
                            video.draw_point(point, color);
                        }

                        yield PpuStep::Cycle;
                    }
                }
            }

            unreachable!();
        }
    }
}

pub enum PpuStep {
    Cycle,
    Vblank,
}

fn nes_color_code_to_rgb(color_code: u8) -> Color {
    // Based on the palette provided on the NesDev wiki:
    // - https://wiki.nesdev.com/w/index.php/PPU_palettes
    // - https://wiki.nesdev.com/w/index.php/File:Savtool-swatches.png
    match color_code & 0x3F {
        0x00 => Color { r: 0x54, g: 0x54, b: 0x54 },
        0x01 => Color { r: 0x00, g: 0x1E, b: 0x74 },
        0x02 => Color { r: 0x08, g: 0x10, b: 0x90 },
        0x03 => Color { r: 0x30, g: 0x00, b: 0x88 },
        0x04 => Color { r: 0x44, g: 0x00, b: 0x64 },
        0x05 => Color { r: 0x5C, g: 0x00, b: 0x30 },
        0x06 => Color { r: 0x54, g: 0x04, b: 0x00 },
        0x07 => Color { r: 0x3C, g: 0x18, b: 0x00 },
        0x08 => Color { r: 0x20, g: 0x2A, b: 0x00 },
        0x09 => Color { r: 0x08, g: 0x3A, b: 0x00 },
        0x0A => Color { r: 0x00, g: 0x40, b: 0x00 },
        0x0B => Color { r: 0x00, g: 0x3C, b: 0x00 },
        0x0C => Color { r: 0x00, g: 0x32, b: 0x3C },
        0x0D => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x0E => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x0F => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x10 => Color { r: 0x98, g: 0x96, b: 0x98 },
        0x11 => Color { r: 0x08, g: 0x4C, b: 0xC4 },
        0x12 => Color { r: 0x30, g: 0x32, b: 0xEC },
        0x13 => Color { r: 0x5C, g: 0x1E, b: 0xE4 },
        0x14 => Color { r: 0x88, g: 0x14, b: 0xB0 },
        0x15 => Color { r: 0xA0, g: 0x14, b: 0x64 },
        0x16 => Color { r: 0x98, g: 0x22, b: 0x20 },
        0x17 => Color { r: 0x78, g: 0x3C, b: 0x00 },
        0x18 => Color { r: 0x54, g: 0x5A, b: 0x00 },
        0x19 => Color { r: 0x28, g: 0x72, b: 0x00 },
        0x1A => Color { r: 0x08, g: 0x7C, b: 0x00 },
        0x1B => Color { r: 0x00, g: 0x76, b: 0x28 },
        0x1C => Color { r: 0x00, g: 0x66, b: 0x78 },
        0x1D => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x1E => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x1F => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x20 => Color { r: 0xEC, g: 0xEE, b: 0xEC },
        0x21 => Color { r: 0x4C, g: 0x9A, b: 0xEC },
        0x22 => Color { r: 0x78, g: 0x7C, b: 0xEC },
        0x23 => Color { r: 0xB0, g: 0x62, b: 0xEC },
        0x24 => Color { r: 0xE4, g: 0x54, b: 0xEC },
        0x25 => Color { r: 0xEC, g: 0x58, b: 0xB4 },
        0x26 => Color { r: 0xEC, g: 0x6A, b: 0x64 },
        0x27 => Color { r: 0xD4, g: 0x88, b: 0x20 },
        0x28 => Color { r: 0xA0, g: 0xAA, b: 0x00 },
        0x29 => Color { r: 0x74, g: 0xC4, b: 0x00 },
        0x2A => Color { r: 0x4C, g: 0xD0, b: 0x20 },
        0x2B => Color { r: 0x38, g: 0xCC, b: 0x6C },
        0x2C => Color { r: 0x38, g: 0xB4, b: 0xCC },
        0x2D => Color { r: 0x3C, g: 0x3C, b: 0x3C },
        0x2E => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x2F => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x30 => Color { r: 0xEC, g: 0xEE, b: 0xEC },
        0x31 => Color { r: 0xA8, g: 0xCC, b: 0xEC },
        0x32 => Color { r: 0xBC, g: 0xBC, b: 0xEC },
        0x33 => Color { r: 0xD4, g: 0xB2, b: 0xEC },
        0x34 => Color { r: 0xEC, g: 0xAE, b: 0xEC },
        0x35 => Color { r: 0xEC, g: 0xAE, b: 0xD4 },
        0x36 => Color { r: 0xEC, g: 0xB4, b: 0xB0 },
        0x37 => Color { r: 0xE4, g: 0xC4, b: 0x90 },
        0x38 => Color { r: 0xCC, g: 0xD2, b: 0x78 },
        0x39 => Color { r: 0xB4, g: 0xDE, b: 0x78 },
        0x3A => Color { r: 0xA8, g: 0xE2, b: 0x90 },
        0x3B => Color { r: 0x98, g: 0xE2, b: 0xB4 },
        0x3C => Color { r: 0xA0, g: 0xD6, b: 0xE4 },
        0x3D => Color { r: 0xA0, g: 0xA2, b: 0xA0 },
        0x3E => Color { r: 0x00, g: 0x00, b: 0x00 },
        0x3F => Color { r: 0x00, g: 0x00, b: 0x00 },
        _ => { unreachable!(); },
    }
}

bitflags! {
    pub struct PpuCtrlFlags: u8 {
        const NAMETABLE_LO = 1 << 0;
        const NAMETABLE_HI = 1 << 1;
        const VRAM_ADDR_INCREMENT = 1 << 2;
        const SPRITE_PATTERN_TABLE_ADDR = 1 << 3;
        const BACKGROUND_PATTERN_TABLE_ADDR = 1 << 4;
        const SPRITE_SIZE = 1 << 5;
        const PPU_MASTER_SLAVE_SELECT = 1 << 6;
        const VBLANK_INTERRUPT = 1 << 7;
    }
}

bitflags! {
    pub struct PpuMaskFlags: u8 {
        const GREYSCALE = 1 << 0;
        const SHOW_BACKGROUND_IN_LEFT_MARGIN = 1 << 1;
        const SHOW_SPRITES_IN_LEFT_MARGIN = 1 << 2;
        const SHOW_BACKGROUND = 1 << 3;
        const SHOW_SPRITES = 1 << 4;
        const EMPHASIZE_RED = 1 << 5;
        const EMPHASIZE_GREEN = 1 << 6;
        const EMPHASIZE_BLUE = 1 << 7;
    }
}

bitflags! {
    pub struct PpuStatusFlags: u8 {
        // NOTE: Bits 0-4 are unused (but result in bits read from
        // the PPU's latch)
        const SPRITE_OVERFLOW = 1 << 5;
        const SPRITE_ZERO_HIT = 1 << 6;
        const VBLANK_STARTED = 1 << 7;
    }
}
