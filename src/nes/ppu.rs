use std::u8;
use std::cell::Cell;
use std::pin::Pin;
use std::ops::{Generator, GeneratorState};
use bitflags::bitflags;
use crate::nes::{Nes, NesIo};
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

    pub ppu_ram: Cell<[u8; 0x0800]>,
    pub oam: Cell<[u8; 0x0100]>,
    pub palette_ram: Cell<[u8; 0x20]>,

    // Internal "cache" indexed by an X coordinate, returning a bitfield
    // that represents the sprites that should be rendered for that
    // X coordinate. For example, a value of 0b_0100_0001 at index 5 means that
    // the sprites at index 0 and 6 should be rendered at pixel 5 of
    // the scanline (since bits 0 and 6 are set)
    scanline_sprite_indices: Cell<[u64; 256]>,
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
            ppu_ram: Cell::new([0; 0x0800]),
            oam: Cell::new([0; 0x0100]),
            palette_ram: Cell::new([0; 0x20]),
            scanline_sprite_indices: Cell::new([0; 256]),
        }
    }

    pub fn ppu_ram(&self) -> &[Cell<u8>] {
        let ppu_ram: &Cell<[u8]> = &self.ppu_ram;
        ppu_ram.as_slice_of_cells()
    }

    fn oam(&self) -> &[Cell<u8>] {
        let oam: &Cell<[u8]> = &self.oam;
        oam.as_slice_of_cells()
    }

    pub fn palette_ram(&self) -> &[Cell<u8>] {
        let palette_ram: &Cell<[u8]> = &self.palette_ram;
        palette_ram.as_slice_of_cells()
    }

    pub fn set_ppuctrl(&self, value: u8) {
        self.ctrl.set(PpuCtrlFlags::from_bits_truncate(value));
    }

    pub fn set_ppumask(&self, value: u8) {
        self.mask.set(PpuMaskFlags::from_bits_truncate(value));
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

    pub fn read_ppudata(&self, nes: &Nes<impl NesIo>) -> u8 {
        let addr = self.addr.get();
        let ctrl = self.ctrl.get();
        let stride =
            // Add 1 to the PPU address if the I flag is clear, add 32 if
            // it is set
            match ctrl.contains(PpuCtrlFlags::VRAM_ADDR_INCREMENT) {
                false => 1,
                true => 32
            };

        let value = nes.read_ppu_u8(addr);
        self.addr.update(|addr| addr.wrapping_add(stride));

        value
    }

    pub fn write_ppudata(&self, nes: &Nes<impl NesIo>, value: u8) {
        let addr = self.addr.get();
        let ctrl = self.ctrl.get();
        let stride =
            // Add 1 to the PPU address if the I flag is clear, add 32 if
            // it is set
            match ctrl.contains(PpuCtrlFlags::VRAM_ADDR_INCREMENT) {
                false => 1,
                true => 32
            };

        nes.write_ppu_u8(addr, value);
        self.addr.update(|addr| addr.wrapping_add(stride));
    }

    pub fn ppustatus(&self) -> u8 {
        self.status.get().bits()
    }

    pub fn run<'a>(nes: &'a Nes<impl NesIo>)
        -> impl Generator<Yield = PpuStep, Return = !> + 'a
    {
        let mut run_sprite_evaluation = Ppu::run_sprite_evaluation(nes);
        let mut run_renderer = Ppu::run_renderer(nes);

        move || loop {
            loop {
                match Pin::new(&mut run_sprite_evaluation).resume() {
                    GeneratorState::Yielded(PpuStep::Cycle) => {
                        break;
                    }
                    GeneratorState::Yielded(step) => {
                        yield step;
                    }
                }
            }
            loop {
                match Pin::new(&mut run_renderer).resume() {
                    GeneratorState::Yielded(PpuStep::Cycle) => {
                        break;
                    }
                    GeneratorState::Yielded(step) => {
                        yield step;
                    }
                }
            }

            yield PpuStep::Cycle;
        }
    }

    fn run_sprite_evaluation<'a>(nes: &'a Nes<impl NesIo>)
        -> impl Generator<Yield = PpuStep, Return = !> + 'a
    {
        move || loop {
            for frame in 0_u64.. {
                let frame_is_odd = frame % 2 != 0;
                for scanline in 0_u16..=261 {
                    let y = scanline;
                    let should_skip_first_cycle = frame_is_odd && scanline == 0;
                    if !should_skip_first_cycle {
                        // The first cycle of each scanline is idle (except
                        // for the first cycle of the pre-render scanline
                        // for odd frames, which is skipped)
                        yield PpuStep::Cycle;
                    }

                    // TODO: Implement sprite evaluation with secondary OAM!

                    let oam = nes.ppu.oam();
                    for _ in 0_u16..340 {
                        // Here, secondary OAM would be filled with sprite data
                        yield PpuStep::Cycle;
                    }

                    if scanline < 240 {
                        let mut new_sprite_indices = [0; 256];
                        for sprite_index in 0_u8..64 {
                            let oam_index = sprite_index as usize * 4;
                            let sprite_y = oam[oam_index].get() as u16;
                            if sprite_y <= y && y < sprite_y + 8 {
                                let sprite_x = oam[oam_index + 3].get() as u16;

                                for sprite_x_offset in 0_u16..8 {
                                    let x = (sprite_x + sprite_x_offset) as usize;
                                    if x < 256 {
                                        let sprite_bitmask = 1 << sprite_index;
                                        new_sprite_indices[x] |= sprite_bitmask;
                                    }
                                }
                            }
                        }

                        nes.ppu.scanline_sprite_indices.set(new_sprite_indices);
                    }
                    else {
                        let new_sprite_indices = [0; 256];
                        nes.ppu.scanline_sprite_indices.set(new_sprite_indices);
                    }
                }
            }
        }
    }

    fn run_renderer<'a>(nes: &'a Nes<impl NesIo>)
        -> impl Generator<Yield = PpuStep, Return = !> + 'a
    {
        move || loop {
            for frame in 0_u64.. {
                let frame_is_odd = frame % 2 != 0;
                for scanline in 0_u16..=261 {
                    let tile_y = scanline / 8;
                    let tile_y_pixel = scanline % 8;
                    let y = scanline;

                    let sprite_indices = nes.ppu.scanline_sprite_indices.get();
                    let oam = nes.ppu.oam();

                    let should_skip_first_cycle = frame_is_odd && scanline == 0;
                    if !should_skip_first_cycle {
                        // The first cycle of each scanline is idle (except
                        // for the first cycle of the pre-render scanline
                        // for odd frames, which is skipped)
                        yield PpuStep::Cycle;
                    }

                    if scanline == 240 {
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
                        nes.io.video().present();
                        yield PpuStep::Vblank;
                    }
                    else if scanline == 0 {
                        let _ = nes.ppu.status.update(|mut status| {
                            status.set(PpuStatusFlags::VBLANK_STARTED, false);
                            status
                        });
                        nes.io.video().clear();
                    }

                    for tile_x in 0_u16..42 {
                        if tile_x >= 32 || scanline >= 240 {
                            // TODO: Implement sprite tile fetching here

                            for _cycle in 0..8 {
                                yield PpuStep::Cycle;
                            }

                            continue;
                        }

                        yield PpuStep::Cycle;
                        yield PpuStep::Cycle;
                        let nametable_index = tile_y * 32 + tile_x;
                        let nametable_byte = nes.read_ppu_u8(0x2000 + nametable_index);

                        yield PpuStep::Cycle;
                        yield PpuStep::Cycle;
                        let attr_x = tile_x / 4;
                        let attr_y = tile_y / 4;
                        let attr_is_left = ((tile_x / 2) % 2) == 0;
                        let attr_is_top = ((tile_y / 2) % 2) == 0;

                        let attr_index = attr_y * 8 + attr_x;
                        let attr = nes.read_ppu_u8(0x23C0 + attr_index);
                        let background_palette_index = match (attr_is_top, attr_is_left) {
                            (true, true)   =>  attr & 0b_0000_0011,
                            (true, false)  => (attr & 0b_0000_1100) >> 2,
                            (false, true)  => (attr & 0b_0011_0000) >> 4,
                            (false, false) => (attr & 0b_1100_0000) >> 6
                        };

                        let pattern_table_offset =
                            if nes.ppu.ctrl.get().contains(PpuCtrlFlags::BACKGROUND_PATTERN_TABLE_ADDR) {
                                0x1000
                            }
                            else {
                                0x0000
                            };
                        let bitmap_offset = pattern_table_offset + nametable_byte as u16 * 16;
                        let bitmap_lo_byte = nes.read_ppu_u8(bitmap_offset + tile_y_pixel);

                        yield PpuStep::Cycle;
                        yield PpuStep::Cycle;
                        let bitmap_hi_byte = nes.read_ppu_u8(bitmap_offset + tile_y_pixel + 8);

                        yield PpuStep::Cycle;
                        yield PpuStep::Cycle;
                        for tile_x_pixel in 0..8 {
                            let x = (tile_x * 8) + tile_x_pixel;

                            let background_color_index = {
                                let bitmap_bitmask = 0b_1000_0000 >> tile_x_pixel;
                                let bitmap_lo_bit = (bitmap_lo_byte & bitmap_bitmask) != 0;
                                let bitmap_hi_bit = (bitmap_hi_byte & bitmap_bitmask) != 0;

                                let color_index = match (bitmap_hi_bit, bitmap_lo_bit) {
                                    (false, false) => 0,
                                    (false, true) => 1,
                                    (true, false) => 2,
                                    (true, true) => 3,
                                };
                                color_index
                            };

                            let sprite_palette_and_color_index = {
                                let sprite_index_bitmask = sprite_indices[x as usize];
                                let included_sprites = (0_u64..64).filter(|sprite_index| {
                                    (sprite_index_bitmask & (1_u64 << sprite_index)) != 0
                                });

                                let mut palette_and_color_indices = included_sprites.map(|sprite_index| {
                                    let oam_index = (sprite_index * 4) as usize;
                                    let sprite_y = oam[oam_index].get() as u16;
                                    let tile_index = oam[oam_index + 1].get();
                                    let attrs = oam[oam_index + 2].get();
                                    let sprite_x = oam[oam_index + 3].get() as u16;

                                    let flip_horizontal = (attrs & 0b_0100_0000) != 0;
                                    let flip_vertical = (attrs & 0b_1000_0000) != 0;

                                    let sprite_x_pixel = x.wrapping_sub(sprite_x) % 256;
                                    let sprite_y_pixel = y.wrapping_sub(1).wrapping_sub(sprite_y) % 256;

                                    let sprite_x_pixel =
                                        if flip_horizontal {
                                            7 - sprite_x_pixel
                                        }
                                        else {
                                            sprite_x_pixel
                                        };
                                    let sprite_y_pixel =
                                        if flip_vertical {
                                            7 - sprite_y_pixel
                                        }
                                        else {
                                            sprite_y_pixel
                                        };

                                    let pattern_bitmask = 0b_1000_0000 >> sprite_x_pixel;
                                    let pattern_table_offset =
                                        if nes.ppu.ctrl.get().contains(PpuCtrlFlags::SPRITE_PATTERN_TABLE_ADDR) {
                                            0x1000
                                        }
                                        else {
                                            0x0000
                                        };
                                    let pattern_offset = pattern_table_offset as u16 + tile_index as u16 * 16;
                                    let pattern_lo_byte = nes.read_ppu_u8(pattern_offset + sprite_y_pixel);
                                    let pattern_hi_byte = nes.read_ppu_u8(pattern_offset + sprite_y_pixel + 8);
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

                                    let color_index = match (pattern_hi_bit, pattern_lo_bit) {
                                        (false, false) => 0,
                                        (false, true) => 1,
                                        (true, false) => 2,
                                        (true, true) => 3,
                                    };

                                    (palette_index, color_index)
                                });

                                let palette_and_color_index = palette_and_color_indices.find(|&(_, color_index)| {
                                    color_index != 0
                                });

                                palette_and_color_index
                            };

                            let color_code = match sprite_palette_and_color_index {
                                None | Some((_, 0)) => {
                                    nes.ppu.palette_index_to_nes_color_code(background_palette_index, background_color_index)
                                }
                                Some((sprite_palette_index, sprite_color_index)) => {
                                    nes.ppu.palette_index_to_nes_color_code(sprite_palette_index, sprite_color_index)
                                }
                            };
                            let color = nes_color_code_to_rgb(color_code);
                            let point = Point { x, y };
                            nes.io.video().draw_point(point, color);
                        }
                    };

                    for _ in 0..4 {
                        // TODO: Implement PPU garbage reads
                        yield PpuStep::Cycle;
                    }
                }
            }
        }
    }

    fn palette_index_to_nes_color_code(
        &self,
        palette_index: u8,
        color_index: u8
    )
        -> u8
    {
        let palette_ram = self.palette_ram();
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
        let palette = palettes[palette_index as usize];
        palette[color_index as usize]
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
