use std::u8;
use std::fmt;
use std::cell::Cell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use bitflags::bitflags;
use enum_kinds::EnumKind;
use crate::rom::Rom;
use crate::video::{Video, Point, Color};

#[derive(Clone)]
pub struct Nes {
    pub rom: Rom,
    pub ram: Cell<[u8; 0x0800]>,
    pub cpu: Cpu,
    pub ppu: Ppu,
}

impl Nes {
    pub fn new_from_rom(rom: Rom) -> Self {
        let ram = Cell::new([0; 0x0800]);
        let cpu = Cpu::new();
        let ppu = Ppu::new();

        let nes = Nes {
            rom,
            ram,
            cpu,
            ppu,
        };

        let reset_addr = nes.read_u16(0xFFFC);

        nes.cpu.pc.set(reset_addr);

        nes
    }

    fn ram(&self) -> &[Cell<u8>] {
        let ram: &Cell<[u8]> = &self.ram;
        ram.as_slice_of_cells()
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        let mapper = self.rom.header.mapper;
        if mapper != 0 {
            unimplemented!("Unhandled mapper: {}", mapper);
        }

        let ram = self.ram();

        match addr {
            0x0000..=0x07FF => {
                ram[addr as usize].get()
            }
            0x2002 => {
                self.ppu.ppustatus()
            }
            0x4016 => {
                // TODO: Return joystick state
                0x40
            }
            0x4017 => {
                // TODO: Return joystick state
                0x40
            }
            0x8000..=0xFFFF => {
                let rom_offset = addr - 0x8000;
                let mapped_addr = rom_offset as usize % self.rom.prg_rom.len();
                self.rom.prg_rom[mapped_addr]
            }
            _ => {
                unimplemented!("Unhandled read from address: 0x{:X}", addr);
            }
        }
    }

    pub fn read_i8(&self, addr: u16) -> i8 {
        self.read_u8(addr) as i8
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read_u8(addr);
        let hi = self.read_u8(addr + 1);

        lo as u16 | ((hi as u16) << 8)
    }

    pub fn write_u8(&self, addr: u16, value: u8) {
        let mapper = self.rom.header.mapper;
        if mapper != 0 {
            unimplemented!("Unhandled mapper: {}", mapper);
        }

        let ram = self.ram();

        match addr {
            0x0000..=0x07FF => {
                ram[addr as usize].set(value);
            }
            0x2000 => {
                self.ppu.set_ppuctrl(value);
            }
            0x2001 => {
                self.ppu.set_ppumask(value);
            }
            0x2003 => {
                self.ppu.write_oamaddr(value);
            }
            0x2005 => {
                self.ppu.write_ppuscroll(value);
            }
            0x2006 => {
                self.ppu.write_ppuaddr(value);
            }
            0x2007 => {
                self.ppu.write_ppudata(value);
            }
            0x4000..=0x4007 => {
                // TODO: APU pulse
            }
            0x4008..=0x400B => {
                // TODO: APU triangle
            }
            0x4014 => {
                self.copy_oam_dma(value);
            }
            0x4015 => {
                // TODO: APU sound channel control
            }
            0x4016 => {
                // TODO: Joystick strobe
            }
            0x4017 => {
                // TODO: Implement APU frame counter
            }
            _ => {
                unimplemented!("Unhandled write to address: 0x{:X}", addr);
            }
        }
    }

    fn push_u8(&self, value: u8) {
        let s = self.cpu.s.get();
        let stack_addr = 0x0100 | s as u16;

        self.write_u8(stack_addr, value);

        self.cpu.s.set(s.wrapping_sub(1));
    }

    fn push_u16(&self, value: u16) {
        let value_hi = ((0xFF00 & value) >> 8) as u8;
        let value_lo = (0x00FF & value) as u8;

        self.push_u8(value_hi);
        self.push_u8(value_lo);
    }

    fn pull_u8(&self) -> u8 {
        let s = self.cpu.s.get().wrapping_add(1);
        self.cpu.s.set(s);

        let stack_addr = 0x0100 | s as u16;

        let value = self.read_u8(stack_addr);

        value
    }

    fn pull_u16(&self) -> u16 {
        let value_lo = self.pull_u8();
        let value_hi = self.pull_u8();

        ((value_hi as u16) << 8) | (value_lo as u16)
    }

    fn copy_oam_dma(&self, page: u8) {
        let target_addr_start = self.ppu.oam_addr.get() as u16;
        let mut oam = self.ppu.oam.get();
        for index in 0x00..=0xFF {
            let source_addr = ((page as u16) << 8) | index;
            let byte = self.read_u8(source_addr);

            let target_addr = (target_addr_start + index) as usize % oam.len();
            oam[target_addr] = byte;
        }

        self.ppu.oam.set(oam);
    }

    pub fn run<'a>(&'a self, video: &'a mut impl Video)
        -> impl Generator<Yield = CpuStep, Return = !> + 'a
    {
        let mut run_cpu = Cpu::run(&self);

        let mut run_ppu = Ppu::run(&self, video);

        move || loop {
            // TODO: Clean this up
            let GeneratorState::Yielded(cpu_step) = Pin::new(&mut run_cpu).resume();
            let GeneratorState::Yielded(()) = Pin::new(&mut run_ppu).resume();
            let GeneratorState::Yielded(()) = Pin::new(&mut run_ppu).resume();
            let GeneratorState::Yielded(()) = Pin::new(&mut run_ppu).resume();

            yield cpu_step;
        }
    }
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

#[derive(Debug, Clone)]
pub struct Cpu {
    pub pc: Cell<u16>,
    pub a: Cell<u8>,
    pub x: Cell<u8>,
    pub y: Cell<u8>,
    pub s: Cell<u8>,
    pub p: Cell<CpuFlags>,
    pub nmi: Cell<bool>,
}

impl Cpu {
    fn new() -> Self {
        Cpu {
            pc: Cell::new(0),
            a: Cell::new(0),
            x: Cell::new(0),
            y: Cell::new(0),
            s: Cell::new(0xFD),
            p: Cell::new(CpuFlags::from_bits_truncate(0x34)),
            nmi: Cell::new(false),
        }
    }

    fn contains_flags(&self, flags: CpuFlags) -> bool {
        self.p.get().contains(flags)
    }

    fn set_flags(&self, flags: CpuFlags, value: bool) {
        // TODO: Prevent the break (`B`) and unused (`U`) flags
        // from being changed!
        let mut p = self.p.get();
        p.set(flags, value);
        self.p.set(p);
    }

    pub fn run<'a>(nes: &'a Nes)
        -> impl Generator<Yield = CpuStep, Return = !> + 'a
    {
        move || loop {
            let nmi = nes.cpu.nmi.get();
            if nmi {
                println!("=== NMI ===");
                nes.cpu.nmi.set(false);

                nes.push_u16(nes.cpu.pc.get());
                nes.push_u8(nes.cpu.p.get().bits);

                let nmi_vector = nes.read_u16(0xFFFA);
                nes.cpu.pc.set(nmi_vector);
            }

            let pc = nes.cpu.pc.get();
            let next_pc;

            let opcode = nes.read_u8(pc);
            let opcode = Opcode::from_u8(opcode);

            let op;
            match opcode {
                Opcode::AdcAbs => {
                    let a = nes.cpu.a.get();
                    let p = nes.cpu.p.get();
                    let c = if p.contains(CpuFlags::C) { 1 } else { 0 };

                    let addr = nes.read_u16(pc + 1);
                    let value = nes.read_u8(addr);

                    let result = a as u16 + c as u16 + value as u16;
                    let out = result as u8;

                    // TODO: Refactor!
                    let signed_result = result as i8;
                    let signed_out = out as i8;
                    let is_sign_correct =
                        (signed_result >= 0 && signed_out >= 0)
                        || (signed_result < 0 && signed_out < 0);

                    nes.cpu.a.set(out);
                    nes.cpu.set_flags(CpuFlags::C, result > u8::MAX as u16);
                    nes.cpu.set_flags(CpuFlags::Z, result == 0);
                    nes.cpu.set_flags(CpuFlags::V, !is_sign_correct);
                    nes.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 3;
                    op = Op::AdcAbs { addr };
                }
                Opcode::AdcImm => {
                    let a = nes.cpu.a.get();
                    let p = nes.cpu.p.get();
                    let c = if p.contains(CpuFlags::C) { 1 } else { 0 };

                    let value = nes.read_u8(pc + 1);

                    let result = a as u16 + c as u16 + value as u16;
                    let out = result as u8;

                    // TODO: Refactor!
                    let signed_result = result as i8;
                    let signed_out = out as i8;
                    let is_sign_correct =
                        (signed_result >= 0 && signed_out >= 0)
                        || (signed_result < 0 && signed_out < 0);

                    nes.cpu.a.set(out);
                    nes.cpu.set_flags(CpuFlags::C, result > u8::MAX as u16);
                    nes.cpu.set_flags(CpuFlags::Z, result == 0);
                    nes.cpu.set_flags(CpuFlags::V, !is_sign_correct);
                    nes.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AdcImm { value };
                }
                Opcode::AdcZero => {
                    let a = nes.cpu.a.get();
                    let p = nes.cpu.p.get();
                    let c = if p.contains(CpuFlags::C) { 1 } else { 0 };

                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    let result = a as u16 + c as u16 + value as u16;
                    let out = result as u8;

                    // TODO: Refactor!
                    let signed_result = result as i8;
                    let signed_out = out as i8;
                    let is_sign_correct =
                        (signed_result >= 0 && signed_out >= 0)
                        || (signed_result < 0 && signed_out < 0);

                    nes.cpu.a.set(out);
                    nes.cpu.set_flags(CpuFlags::C, result > u8::MAX as u16);
                    nes.cpu.set_flags(CpuFlags::Z, result == 0);
                    nes.cpu.set_flags(CpuFlags::V, !is_sign_correct);
                    nes.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AdcZero { zero_page };
                }
                Opcode::AndImm => {
                    let a = nes.cpu.a.get();
                    let value = nes.read_u8(pc + 1);
                    let a = a & value;
                    nes.cpu.a.set(a);

                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AndImm { value };
                }
                Opcode::AndZero => {
                    let a = nes.cpu.a.get();
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);
                    let a = a & value;
                    nes.cpu.a.set(a);

                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AndZero { zero_page };
                }
                Opcode::AndZeroX => {
                    let a = nes.cpu.a.get();
                    let x = nes.cpu.x.get();
                    let zero_page_base = nes.read_u8(pc + 1);
                    let addr = (zero_page_base as u16).wrapping_add(x as u16);
                    let value = nes.read_u8(addr);
                    let a = a & value;
                    nes.cpu.a.set(a);

                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AndZeroX { zero_page_base };
                }
                Opcode::AslA => {
                    let a = nes.cpu.a.get();

                    let c = (a & 0b_1000_0000) != 0;
                    let a = a << 1;
                    nes.cpu.a.set(a);

                    nes.cpu.set_flags(CpuFlags::C, c);
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::AslA;
                }
                Opcode::AslZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    let c = (value & 0b_1000_0000) != 0;
                    let new_value = value << 1;
                    nes.write_u8(addr, new_value);

                    nes.cpu.set_flags(CpuFlags::C, c);
                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AslZero { zero_page };
                }
                Opcode::Bcc => {
                    let addr_offset = nes.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if nes.cpu.contains_flags(CpuFlags::C) {
                        next_pc = pc_after;
                    }
                    else {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    op = Op::Bcc { addr_offset };
                }
                Opcode::Bcs => {
                    let addr_offset = nes.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if !nes.cpu.contains_flags(CpuFlags::C) {
                        next_pc = pc_after;
                    }
                    else {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    op = Op::Bcs { addr_offset };
                }
                Opcode::Beq => {
                    let addr_offset = nes.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if nes.cpu.contains_flags(CpuFlags::Z) {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    else {
                        next_pc = pc_after;
                    }
                    op = Op::Beq { addr_offset };
                }
                Opcode::Bmi => {
                    let addr_offset = nes.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if !nes.cpu.contains_flags(CpuFlags::N) {
                        next_pc = pc_after;
                    }
                    else {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    op = Op::Bmi { addr_offset };
                }
                Opcode::Bne => {
                    let addr_offset = nes.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if nes.cpu.contains_flags(CpuFlags::Z) {
                        next_pc = pc_after;
                    }
                    else {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    op = Op::Bne { addr_offset };
                }
                Opcode::Bpl => {
                    let addr_offset = nes.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if nes.cpu.contains_flags(CpuFlags::N) {
                        next_pc = pc_after;
                    }
                    else {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    op = Op::Bpl { addr_offset };
                }
                Opcode::Clc => {
                    nes.cpu.set_flags(CpuFlags::C, false);

                    next_pc = pc + 1;
                    op = Op::Clc;
                }
                Opcode::Cld => {
                    nes.cpu.set_flags(CpuFlags::D, false);
                    next_pc = pc + 1;
                    op = Op::Cld;
                }
                Opcode::CmpImm => {
                    let value = nes.read_u8(pc + 1);
                    let a = nes.cpu.a.get();
                    let result = a.wrapping_sub(value);

                    nes.cpu.set_flags(CpuFlags::C, a >= value);
                    nes.cpu.set_flags(CpuFlags::Z, a == value);
                    nes.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::CmpImm { value };
                }
                Opcode::DecAbs => {
                    let addr = nes.read_u16(pc + 1);
                    let value = nes.read_u8(addr);
                    let value = value.wrapping_sub(1);
                    nes.write_u8(addr, value);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 3;
                    op = Op::DecAbs { addr };
                }
                Opcode::DecZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);
                    let value = value.wrapping_sub(1);
                    nes.write_u8(addr, value);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::DecZero { zero_page };
                }
                Opcode::DecZeroX => {
                    let x = nes.cpu.x.get();
                    let zero_page_base = nes.read_u8(pc + 1);
                    let addr = (zero_page_base as u16).wrapping_add(x as u16);
                    let value = nes.read_u8(addr);
                    let value = value.wrapping_sub(1);
                    nes.write_u8(addr, value);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::DecZeroX { zero_page_base };
                }
                Opcode::Dex => {
                    let x = nes.cpu.x.get().wrapping_sub(1);
                    nes.cpu.set_flags(CpuFlags::Z, x == 0);
                    nes.cpu.set_flags(CpuFlags::N, (x & 0b_1000_0000) != 0);
                    nes.cpu.x.set(x);

                    next_pc = pc + 1;
                    op = Op::Dex;
                }
                Opcode::Dey => {
                    let y = nes.cpu.y.get().wrapping_sub(1);
                    nes.cpu.set_flags(CpuFlags::Z, y == 0);
                    nes.cpu.set_flags(CpuFlags::N, (y & 0b_1000_0000) != 0);
                    nes.cpu.y.set(y);

                    next_pc = pc + 1;
                    op = Op::Dey;
                }
                Opcode::EorImm => {
                    let value = nes.read_u8(pc + 1);
                    let a = nes.cpu.a.get() ^ value;
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);
                    nes.cpu.a.set(a);

                    next_pc = pc + 2;
                    op = Op::EorImm { value };
                }
                Opcode::EorZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);
                    let a = nes.cpu.a.get() ^ value;
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);
                    nes.cpu.a.set(a);

                    next_pc = pc + 2;
                    op = Op::EorZero { zero_page };
                }
                Opcode::IncZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);
                    let value = value.wrapping_add(1);
                    nes.write_u8(addr, value);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::IncZero { zero_page };
                }
                Opcode::Inx => {
                    let x = nes.cpu.x.get().wrapping_add(1);
                    nes.cpu.set_flags(CpuFlags::Z, x == 0);
                    nes.cpu.set_flags(CpuFlags::N, (x & 0b_1000_0000) != 0);
                    nes.cpu.x.set(x);

                    next_pc = pc + 1;
                    op = Op::Inx;
                }
                Opcode::Iny => {
                    let y = nes.cpu.y.get().wrapping_add(1);
                    nes.cpu.set_flags(CpuFlags::Z, y == 0);
                    nes.cpu.set_flags(CpuFlags::N, (y & 0b_1000_0000) != 0);
                    nes.cpu.y.set(y);

                    next_pc = pc + 1;
                    op = Op::Iny;
                }
                Opcode::JmpAbs => {
                    let addr = nes.read_u16(pc + 1);

                    next_pc = addr;
                    op = Op::JmpAbs { addr };
                }
                Opcode::Jsr => {
                    let addr = nes.read_u16(pc + 1);
                    let ret_pc = pc.wrapping_add(3);
                    let push_pc = ret_pc.wrapping_sub(1);

                    nes.push_u16(push_pc);

                    next_pc = addr;
                    op = Op::Jsr { addr };
                }
                Opcode::LdaAbs => {
                    let addr = nes.read_u16(pc + 1);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 3;
                    op = Op::LdaAbs { addr };
                }
                Opcode::LdaAbsX => {
                    let x = nes.cpu.x.get();
                    let addr_base = nes.read_u16(pc + 1);
                    let addr = addr_base.wrapping_add(x as u16);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 3;
                    op = Op::LdaAbsX { addr_base };
                }
                Opcode::LdaAbsY => {
                    let y = nes.cpu.y.get();
                    let addr_base = nes.read_u16(pc + 1);
                    let addr = addr_base.wrapping_add(y as u16);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 3;
                    op = Op::LdaAbsY { addr_base };
                }
                Opcode::LdaImm => {
                    let value = nes.read_u8(pc + 1);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 2;
                    op = Op::LdaImm { value };
                }
                Opcode::LdaIndY => {
                    let y = nes.cpu.y.get();
                    let target_addr_base = nes.read_u8(pc + 1);

                    // TODO: Is this right? Does the target address
                    // wrap around the zero page?
                    let addr_base = nes.read_u16(target_addr_base as u16);
                    let addr = addr_base.wrapping_add(y as u16);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 2;
                    op = Op::LdaIndY { target_addr_base };
                }
                Opcode::LdaZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 2;
                    op = Op::LdaZero { zero_page };
                }
                Opcode::LdaZeroX => {
                    let x = nes.cpu.x.get();
                    let zero_page_base = nes.read_u8(pc + 1);
                    let addr = (zero_page_base as u16).wrapping_add(x as u16);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.a.set(value);

                    next_pc = pc + 2;
                    op = Op::LdaZeroX { zero_page_base };
                }
                Opcode::LdxAbs => {
                    let addr = nes.read_u16(pc + 1);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.x.set(value);

                    next_pc = pc + 3;
                    op = Op::LdxAbs { addr };
                }
                Opcode::LdxImm => {
                    let value = nes.read_u8(pc + 1);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.x.set(value);

                    next_pc = pc + 2;
                    op = Op::LdxImm { value };
                }
                Opcode::LdxZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.x.set(value);

                    next_pc = pc + 2;
                    op = Op::LdxZero { zero_page };
                }
                Opcode::LdyImm => {
                    let value = nes.read_u8(pc + 1);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.y.set(value);

                    next_pc = pc + 2;
                    op = Op::LdyImm { value };
                }
                Opcode::LdyZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.y.set(value);

                    next_pc = pc + 2;
                    op = Op::LdyZero { zero_page };
                }
                Opcode::LdyZeroX => {
                    let x = nes.cpu.x.get();

                    let zero_page_base = nes.read_u8(pc + 1);
                    let addr = (zero_page_base as u16).wrapping_add(x as u16);
                    let value = nes.read_u8(addr);

                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
                    nes.cpu.y.set(value);

                    next_pc = pc + 2;
                    op = Op::LdyZeroX { zero_page_base };
                }
                Opcode::LsrA => {
                    let a = nes.cpu.a.get();
                    let carry = (a & 0b_0000_0001) != 0;

                    let result = a >> 1;
                    nes.cpu.a.set(result);
                    nes.cpu.set_flags(CpuFlags::C, carry);
                    nes.cpu.set_flags(CpuFlags::Z, result == 0);
                    nes.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::LsrA;
                }
                Opcode::LsrZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    let c = (value & 0b_1000_0000) != 0;
                    let new_value = value >> 1;
                    nes.write_u8(addr, new_value);

                    nes.cpu.set_flags(CpuFlags::C, c);
                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::LsrZero { zero_page };
                }
                Opcode::OraImm => {
                    let value = nes.read_u8(pc + 1);
                    let a = nes.cpu.a.get() | value;
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);
                    nes.cpu.a.set(a);

                    next_pc = pc + 2;
                    op = Op::OraImm { value };
                }
                Opcode::OraZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);
                    let a = nes.cpu.a.get() | value;
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);
                    nes.cpu.a.set(a);

                    next_pc = pc + 2;
                    op = Op::OraZero { zero_page };
                }
                Opcode::Pha => {
                    let a = nes.cpu.a.get();
                    nes.push_u8(a);
                    next_pc = pc + 1;
                    op = Op::Pha;
                }
                Opcode::Pla => {
                    let a = nes.pull_u8();
                    nes.cpu.a.set(a);

                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::Pla;
                }
                Opcode::RolA => {
                    let value = nes.cpu.a.get();

                    let prev_c = nes.cpu.contains_flags(CpuFlags::C);
                    let carry_mask = match prev_c {
                        true  => 0b_0000_0001,
                        false => 0b_0000_0000,
                    };

                    let c = (value & 0b_1000_0000) != 0;
                    let new_value = (value << 1) | carry_mask;
                    nes.cpu.a.set(new_value);

                    nes.cpu.set_flags(CpuFlags::C, c);
                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::RolA;
                }
                Opcode::RorA => {
                    let value = nes.cpu.a.get();

                    let prev_c = nes.cpu.contains_flags(CpuFlags::C);
                    let carry_mask = match prev_c {
                        true  => 0b_1000_0000,
                        false => 0b_0000_0000,
                    };

                    let c = (value & 0b_0000_0001) != 0;
                    let new_value = (value >> 1) | carry_mask;
                    nes.cpu.a.set(new_value);

                    nes.cpu.set_flags(CpuFlags::C, c);
                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::RorA;
                }
                Opcode::RorZero => {
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = nes.read_u8(addr);

                    let prev_c = nes.cpu.contains_flags(CpuFlags::C);
                    let carry_mask = match prev_c {
                        true  => 0b_1000_0000,
                        false => 0b_0000_0000,
                    };

                    let c = (value & 0b_0000_0001) != 0;
                    let new_value = (value >> 1) | carry_mask;
                    nes.write_u8(addr, new_value);

                    nes.cpu.set_flags(CpuFlags::C, c);
                    nes.cpu.set_flags(CpuFlags::Z, value == 0);
                    nes.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::RorZero { zero_page };
                }
                Opcode::Rti => {
                    let ret_p = nes.pull_u8();
                    let ret_pc = nes.pull_u16();

                    nes.cpu.p.set(CpuFlags::from_bits_truncate(ret_p));

                    next_pc = ret_pc;
                    op = Op::Rti;
                }
                Opcode::Rts => {
                    let push_pc = nes.pull_u16();
                    let ret_pc = push_pc.wrapping_add(1);

                    next_pc = ret_pc;
                    op = Op::Rts;
                }
                Opcode::Sec => {
                    nes.cpu.set_flags(CpuFlags::C, true);
                    next_pc = pc + 1;
                    op = Op::Sec;
                }
                Opcode::Sei => {
                    nes.cpu.set_flags(CpuFlags::I, true);
                    next_pc = pc + 1;
                    op = Op::Sei;
                }
                Opcode::StaAbs => {
                    let a = nes.cpu.a.get();
                    let addr = nes.read_u16(pc + 1);
                    nes.write_u8(addr, a);
                    next_pc = pc + 3;
                    op = Op::StaAbs { addr };
                }
                Opcode::StaAbsX => {
                    let a = nes.cpu.a.get();
                    let x = nes.cpu.x.get();
                    let addr_base = nes.read_u16(pc + 1);
                    let addr = addr_base.wrapping_add(x as u16);
                    nes.write_u8(addr, a);
                    next_pc = pc + 3;
                    op = Op::StaAbsX { addr_base };
                }
                Opcode::StaIndY => {
                    let a = nes.cpu.a.get();
                    let y = nes.cpu.y.get();
                    let target_addr_base = nes.read_u8(pc + 1);

                    // TODO: Is this right? Does the target address
                    // wrap around the zero page?
                    let addr_base = nes.read_u16(target_addr_base as u16);
                    let addr = addr_base.wrapping_add(y as u16);
                    nes.write_u8(addr, a);

                    next_pc = pc + 2;
                    op = Op::StaIndY { target_addr_base };
                }
                Opcode::StaZero => {
                    let a = nes.cpu.a.get();
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    nes.write_u8(addr, a);
                    next_pc = pc + 2;
                    op = Op::StaZero { zero_page };
                }
                Opcode::StaZeroX => {
                    let a = nes.cpu.a.get();
                    let x = nes.cpu.x.get();
                    let zero_page_base = nes.read_u8(pc + 1);
                    let addr = (zero_page_base as u16).wrapping_add(x as u16);
                    nes.write_u8(addr, a);
                    next_pc = pc + 2;
                    op = Op::StaZeroX { zero_page_base };
                }
                Opcode::StxAbs => {
                    let x = nes.cpu.x.get();
                    let addr = nes.read_u16(pc + 1);
                    nes.write_u8(addr, x);
                    next_pc = pc + 3;
                    op = Op::StxAbs { addr };
                }
                Opcode::StxZero => {
                    let x = nes.cpu.x.get();
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    nes.write_u8(addr, x);
                    next_pc = pc + 2;
                    op = Op::StxZero { zero_page };
                }
                Opcode::StyAbs => {
                    let y = nes.cpu.y.get();
                    let addr = nes.read_u16(pc + 1);
                    nes.write_u8(addr, y);
                    next_pc = pc + 3;
                    op = Op::StyAbs { addr };
                }
                Opcode::StyZero => {
                    let y = nes.cpu.y.get();
                    let zero_page = nes.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    nes.write_u8(addr, y);
                    next_pc = pc + 2;
                    op = Op::StyZero { zero_page };
                }
                Opcode::Tax => {
                    let a = nes.cpu.a.get();
                    nes.cpu.x.set(a);
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::Tax;
                }
                Opcode::Tay => {
                    let a = nes.cpu.a.get();
                    nes.cpu.y.set(a);
                    nes.cpu.set_flags(CpuFlags::Z, a == 0);
                    nes.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::Tay;
                }
                Opcode::Txa => {
                    let x = nes.cpu.x.get();
                    nes.cpu.a.set(x);
                    next_pc = pc + 1;
                    op = Op::Txa;
                }
                Opcode::Txs => {
                    let x = nes.cpu.x.get();
                    nes.cpu.s.set(x);
                    next_pc = pc + 1;
                    op = Op::Txs;
                }
                Opcode::Tya => {
                    let y = nes.cpu.y.get();
                    nes.cpu.a.set(y);
                    next_pc = pc + 1;
                    op = Op::Tya;
                }
            }

            nes.cpu.pc.set(next_pc);

            debug_assert_eq!(Opcode::from(&op), opcode);

            yield CpuStep { pc, op };
        }
    }
}

bitflags! {
    pub struct CpuFlags: u8 {
        /// Carry flag: set when an arithmetic operation resulted in a carry.
        const C = 1 << 0;

        /// Zero flag: set when an operation results in 0.
        const Z = 1 << 1;

        /// Interrupt disable flag: set to disable CPU interrupts.
        const I = 1 << 2;

        /// Decimal mode flag: exists for compatibility with the 6502 (which
        /// used it for decimal arithmetic), but ignored by the
        /// Rioch 2A03/2A07 processors used in the NES/Famicom.
        const D = 1 << 3;

        /// Break flag: set when BRK or PHP are called, cleared when
        /// the /IRQ or /NMI interrupts are called. When PLP or RTI are called,
        /// this flag is unaffected.
        ///
        /// In other words, this flag isn't exactly "set" or "cleared"-- when an
        /// interrupt happens, the status register is pushed to the stack. If
        /// the interrupt was an /IRQ or /NMI interrupt, the value pushed to the
        /// stack will have this bit cleared; if the interrupt was caused by
        /// BRK (or if the PHP instruction is used), then the value pushed to
        /// the stack will have this bit set.
        const B = 1 << 4;

        /// Unused: this flag is always set to 1
        const U = 1 << 5;

        /// Overflow flag: set when an operation resulted in a signed overflow.
        const V = 1 << 6;

        /// Negative flag: set when an operation resulted in a negative value,
        /// i.e. when the most significant bit (bit 7) is set.
        const N = 1 << 7;
    }
}

#[derive(Debug, EnumKind)]
#[enum_kind(Opcode)]
pub enum Op {
    AdcImm { value: u8 },
    AdcZero { zero_page: u8 },
    AdcAbs { addr: u16 },
    AndImm { value: u8 },
    AndZero { zero_page: u8 },
    AndZeroX { zero_page_base: u8 },
    AslA,
    AslZero { zero_page: u8 },
    Bcc { addr_offset: i8 },
    Bcs { addr_offset: i8 },
    Beq { addr_offset: i8 },
    Bmi { addr_offset: i8 },
    Bne { addr_offset: i8 },
    Bpl { addr_offset: i8 },
    Clc,
    Cld,
    CmpImm { value: u8 },
    DecAbs { addr: u16 },
    DecZero { zero_page: u8 },
    DecZeroX { zero_page_base: u8 },
    Dex,
    Dey,
    EorImm { value: u8 },
    EorZero { zero_page: u8 },
    IncZero { zero_page: u8 },
    Inx,
    Iny,
    JmpAbs { addr: u16 },
    Jsr { addr: u16 },
    LdaAbs { addr: u16 },
    LdaAbsX { addr_base: u16 },
    LdaAbsY { addr_base: u16 },
    LdaImm { value: u8 },
    LdaZero { zero_page: u8 },
    LdaZeroX { zero_page_base: u8 },
    LdaIndY { target_addr_base: u8 },
    LdxAbs { addr: u16 },
    LdxImm { value: u8 },
    LdxZero { zero_page: u8 },
    LdyImm { value: u8 },
    LdyZero { zero_page: u8 },
    LdyZeroX { zero_page_base: u8 },
    LsrA,
    LsrZero { zero_page: u8 },
    OraImm { value: u8 },
    OraZero { zero_page: u8 },
    Pha,
    Pla,
    RolA,
    RorA,
    RorZero { zero_page: u8 },
    Rti,
    Rts,
    Sec,
    Sei,
    StaAbs { addr: u16 },
    StaAbsX { addr_base: u16 },
    StaZero { zero_page: u8 },
    StaZeroX { zero_page_base: u8},
    StaIndY { target_addr_base: u8 },
    StxAbs { addr: u16 },
    StxZero { zero_page: u8 },
    StyAbs { addr: u16 },
    StyZero { zero_page: u8 },
    Tax,
    Tay,
    Txa,
    Txs,
    Tya,
}

impl Opcode {
    fn from_u8(opcode: u8) -> Self {
        match opcode {
            0x05 => Opcode::OraZero,
            0x06 => Opcode::AslZero,
            0x09 => Opcode::OraImm,
            0x0A => Opcode::AslA,
            0x10 => Opcode::Bpl,
            0x18 => Opcode::Clc,
            0x20 => Opcode::Jsr,
            0x25 => Opcode::AndZero,
            0x29 => Opcode::AndImm,
            0x2A => Opcode::RolA,
            0x30 => Opcode::Bmi,
            0x35 => Opcode::AndZeroX,
            0x38 => Opcode::Sec,
            0x40 => Opcode::Rti,
            0x45 => Opcode::EorZero,
            0x46 => Opcode::LsrZero,
            0x48 => Opcode::Pha,
            0x49 => Opcode::EorImm,
            0x4A => Opcode::LsrA,
            0x4C => Opcode::JmpAbs,
            0x60 => Opcode::Rts,
            0x65 => Opcode::AdcZero,
            0x66 => Opcode::RorZero,
            0x68 => Opcode::Pla,
            0x69 => Opcode::AdcImm,
            0x6A => Opcode::RorA,
            0x6D => Opcode::AdcAbs,
            0x78 => Opcode::Sei,
            0x8A => Opcode::Txa,
            0x84 => Opcode::StyZero,
            0x85 => Opcode::StaZero,
            0x86 => Opcode::StxZero,
            0x88 => Opcode::Dey,
            0x8C => Opcode::StyAbs,
            0x8D => Opcode::StaAbs,
            0x8E => Opcode::StxAbs,
            0x90 => Opcode::Bcc,
            0x91 => Opcode::StaIndY,
            0x95 => Opcode::StaZeroX,
            0x98 => Opcode::Tya,
            0x9A => Opcode::Txs,
            0x9D => Opcode::StaAbsX,
            0xA0 => Opcode::LdyImm,
            0xA2 => Opcode::LdxImm,
            0xA4 => Opcode::LdyZero,
            0xA5 => Opcode::LdaZero,
            0xA6 => Opcode::LdxZero,
            0xA8 => Opcode::Tay,
            0xA9 => Opcode::LdaImm,
            0xAA => Opcode::Tax,
            0xAD => Opcode::LdaAbs,
            0xAE => Opcode::LdxAbs,
            0xB0 => Opcode::Bcs,
            0xB1 => Opcode::LdaIndY,
            0xB4 => Opcode::LdyZeroX,
            0xB5 => Opcode::LdaZeroX,
            0xB9 => Opcode::LdaAbsY,
            0xBD => Opcode::LdaAbsX,
            0xC6 => Opcode::DecZero,
            0xC8 => Opcode::Iny,
            0xC9 => Opcode::CmpImm,
            0xCA => Opcode::Dex,
            0xCE => Opcode::DecAbs,
            0xD0 => Opcode::Bne,
            0xD6 => Opcode::DecZeroX,
            0xD8 => Opcode::Cld,
            0xE6 => Opcode::IncZero,
            0xE8 => Opcode::Inx,
            0xF0 => Opcode::Beq,
            opcode => {
                unimplemented!("Unhandled opcode: 0x{:X}", opcode);
            }
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            Opcode::AdcAbs
            | Opcode::AdcImm
            | Opcode::AdcZero => "ADC",
            Opcode::AndImm
            | Opcode::AndZero
            | Opcode::AndZeroX => "AND",
            Opcode::AslA
            | Opcode::AslZero => "ASL",
            Opcode::Bcc => "BCC",
            Opcode::Bcs => "BCS",
            Opcode::Beq => "BEQ",
            Opcode::Bmi => "BMI",
            Opcode::Bne => "BNE",
            Opcode::Bpl => "BPL",
            Opcode::Clc => "CLC",
            Opcode::Cld => "CLD",
            Opcode::CmpImm => "CMP",
            Opcode::DecAbs
            | Opcode::DecZero
            | Opcode::DecZeroX => "DEC",
            Opcode::Dex => "DEX",
            Opcode::Dey => "DEY",
            Opcode::EorImm | Opcode::EorZero => "EOR",
            Opcode::IncZero => "INC",
            Opcode::Inx => "INX",
            Opcode::Iny => "INY",
            Opcode::JmpAbs => "JMP",
            Opcode::Jsr => "JSR",
            Opcode::LdaAbs
            | Opcode::LdaAbsX
            | Opcode::LdaAbsY
            | Opcode::LdaImm
            | Opcode::LdaIndY
            | Opcode::LdaZero
            | Opcode::LdaZeroX => "LDA",
            Opcode::LdxAbs
            | Opcode::LdxImm
            | Opcode::LdxZero => "LDX",
            Opcode::LdyImm
            | Opcode::LdyZero
            | Opcode::LdyZeroX => "LDY",
            Opcode::LsrA
            | Opcode::LsrZero => "LSR",
            Opcode::OraZero
            | Opcode::OraImm => "ORA",
            Opcode::Pha => "PHA",
            Opcode::Pla => "PLA",
            Opcode::RolA => "ROL",
            Opcode::RorA
            | Opcode::RorZero => "ROR",
            Opcode::Rti => "RTI",
            Opcode::Rts => "RTS",
            Opcode::Sec => "SEC",
            Opcode::Sei => "SEI",
            Opcode::StaAbs
            | Opcode::StaAbsX
            | Opcode::StaIndY
            | Opcode::StaZero
            | Opcode::StaZeroX => "STA",
            Opcode::StxAbs
            | Opcode::StxZero => "STX",
            Opcode::StyAbs
            | Opcode::StyZero => "STY",
            Opcode::Tax => "TAX",
            Opcode::Tay => "TAY",
            Opcode::Txa => "TXA",
            Opcode::Txs => "TXS",
            Opcode::Tya => "TYA",
        };
        write!(f, "{}", mnemonic)?;
        Ok(())
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let opcode = Opcode::from(self);
        match self {
            Op::Clc
            | Op::Cld
            | Op::Dex
            | Op::Dey
            | Op::Inx
            | Op::Iny
            | Op::Pha
            | Op::Pla
            | Op::Rti
            | Op::Rts
            | Op::Sec
            | Op::Sei
            | Op::Tax
            | Op::Tay
            | Op::Txa
            | Op::Txs
            | Op::Tya => {
                write!(f, "{}", opcode)?;
            }
            Op::AslA
            | Op::LsrA
            | Op::RolA
            | Op::RorA => {
                write!(f, "{} A", opcode)?;
            }
            Op::AdcAbs { addr}
            | Op::DecAbs { addr }
            | Op::LdaAbs { addr }
            | Op::LdxAbs { addr }
            | Op::StaAbs { addr }
            | Op::StxAbs { addr }
            | Op::StyAbs { addr }
            | Op::JmpAbs { addr }
            | Op::Jsr { addr } => {
                write!(f, "{} ${:04X}", opcode, addr)?;
            }
            Op::LdaAbsX { addr_base }
            | Op::StaAbsX { addr_base } => {
                write!(f, "{} ${:04X},X", opcode, addr_base)?;
            }
            Op::LdaAbsY { addr_base } => {
                write!(f, "{} ${:04X},Y", opcode, addr_base)?;
            }
            Op::AdcZero { zero_page }
            | Op::AndZero { zero_page }
            | Op::AslZero { zero_page }
            | Op::DecZero { zero_page }
            | Op::EorZero { zero_page }
            | Op::IncZero { zero_page }
            | Op::LdaZero { zero_page }
            | Op::LdxZero { zero_page }
            | Op::LdyZero { zero_page }
            | Op::LsrZero { zero_page }
            | Op::OraZero { zero_page }
            | Op::RorZero { zero_page }
            | Op::StaZero { zero_page }
            | Op::StxZero { zero_page }
            | Op::StyZero { zero_page } => {
                write!(f, "{} ${:04X}", opcode, *zero_page as u16)?;
            }
            Op::AndZeroX { zero_page_base }
            | Op::DecZeroX { zero_page_base }
            | Op::LdaZeroX { zero_page_base }
            | Op::LdyZeroX { zero_page_base }
            | Op::StaZeroX { zero_page_base } => {
                write!(f, "{} ${:04X},X", opcode, zero_page_base)?;
            }
            Op::LdaIndY { target_addr_base }
            | Op::StaIndY { target_addr_base } => {
                write!(f, "{} (${:02X}),Y", opcode, target_addr_base)?;
            }
            Op::AdcImm { value }
            | Op::AndImm { value }
            | Op::CmpImm { value }
            | Op::EorImm { value }
            | Op::LdaImm { value }
            | Op::LdxImm { value }
            | Op::LdyImm { value }
            | Op::OraImm { value } => {
                write!(f, "{} #${:02X}", opcode, value)?;
            }
            Op::Bcc { addr_offset }
            | Op::Bcs { addr_offset }
            | Op::Beq { addr_offset }
            | Op::Bmi { addr_offset }
            | Op::Bne { addr_offset }
            | Op::Bpl { addr_offset } => {
                if *addr_offset >= 0 {
                    write!(f, "{} _ + #${:02X}", opcode, addr_offset)?;
                }
                else {
                    let abs_offset = -(*addr_offset as i16);
                    write!(f, "{} _ - #${:02X}", opcode, abs_offset)?;
                }
            }
        }
        Ok(())
    }
}

pub struct CpuStep {
    pub pc: u16,
    pub op: Op,
}

#[derive(Clone)]
pub struct Ppu {
    ctrl: Cell<PpuCtrlFlags>,
    mask: Cell<PpuMaskFlags>,
    status: Cell<PpuStatusFlags>,
    oam_addr: Cell<u8>,
    scroll: Cell<u16>,
    addr: Cell<u16>,

    // Latch used for writing to PPUSCROLL and PPUADDR (toggles after a write
    // to each, used to determine if the high bit or low bit is being written).
    scroll_addr_latch: Cell<bool>,

    nametables: Cell<[u8; 4 * 0x0400]>,
    oam: Cell<[u8; 0x0100]>,
    palette_ram: Cell<[u8; 0x20]>,
}

impl Ppu {
    fn new() -> Self {
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

    fn palette_ram(&self) -> &[Cell<u8>] {
        let palette_ram: &Cell<[u8]> = &self.palette_ram;
        palette_ram.as_slice_of_cells()
    }

    fn set_ppuctrl(&self, value: u8) {
        self.ctrl.set(PpuCtrlFlags::from_bits_truncate(value));
    }

    fn set_ppumask(&self, value: u8) {
        self.mask.set(PpuMaskFlags::from_bits_truncate(value));
    }

    fn write_addr(&self, addr: u16, value: u8) {
        let nametables = self.nametables();
        let palette_ram = self.palette_ram();

        match addr {
            0x0000..=0x0FFF => {
                unimplemented!();
            }
            0x2000..=0x23FF => {
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

    fn write_oamaddr(&self, value: u8) {
        self.oam_addr.set(value);
    }

    fn write_ppuscroll(&self, value: u8) {
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

    fn write_ppuaddr(&self, value: u8) {
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

    fn write_ppudata(&self, value: u8) {
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

    fn ppustatus(&self) -> u8 {
        self.status.get().bits()
    }

    pub fn run<'a, 'b>(nes: &'a Nes, video: &'a mut impl Video)
        -> impl Generator<Yield = (), Return = !> + 'a
    {
        move || {
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
                            nes.cpu.nmi.set(true);
                            video.present();
                        }
                        else if scanline == 0 && cycle == 1 {
                            let _ = nes.ppu.status.update(|mut status| {
                                status.set(PpuStatusFlags::VBLANK_STARTED, false);
                                status
                            });
                            video.clear();
                        }

                        if 0 < scanline && scanline < 240 && 0 < cycle && cycle < 256 {
                            let x = cycle;
                            let y = scanline - 1;

                            let tile_x = x / 8;
                            let tile_y = y / 8;

                            let tile_x_pixel = x % 8;
                            let tile_y_pixel = y % 8;

                            let attr_x = x / 32;
                            let attr_y = y / 32;
                            let attr_is_left = (tile_x % 2) == 0;
                            let attr_is_top = (tile_y % 2) == 0;

                            let nametables = nes.ppu.nametables();
                            let nametable = &nametables[0x000..0x400];
                            let tile_index = (tile_y * 32 + tile_x) as usize;
                            let tile = nametable[tile_index].get();

                            let palette_ram = nes.ppu.palette_ram();
                            let attr_index = (attr_y * 8  + attr_x) as usize;
                            let attr = nametable[0x3C0 + attr_index].get();
                            let palette_index = match (attr_is_top, attr_is_left) {
                                (true, true)   =>  attr & 0b_0000_0011,
                                (true, false)  => (attr & 0b_0000_1100) >> 2,
                                (false, true)  => (attr & 0b_0011_0000) >> 4,
                                (false, false) => (attr & 0b_1100_0000) >> 6
                            };
                            let palette_ram_indices = match palette_index {
                                0 => [0x00, 0x01, 0x02, 0x03],
                                1 => [0x00, 0x05, 0x06, 0x07],
                                2 => [0x00, 0x09, 0x0A, 0x0B],
                                3 => [0x00, 0x0D, 0x0E, 0x0F],
                                _ => { unreachable!(); }
                            };
                            let palette = [
                                palette_ram[palette_ram_indices[0]].get(),
                                palette_ram[palette_ram_indices[1]].get(),
                                palette_ram[palette_ram_indices[2]].get(),
                                palette_ram[palette_ram_indices[3]].get(),
                            ];


                            let pattern_bitmask = 0b_1000_0000 >> tile_x_pixel;
                            let pattern_tables = &nes.rom.chr_rom;

                            let ppu_ctrl = nes.ppu.ctrl.get();
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

                            let color_code = match (pattern_lo_bit, pattern_hi_bit) {
                                (false, false) => palette[0],
                                (false, true) => palette[1],
                                (true, false) => palette[2],
                                (true, true) => palette[3],
                            };
                            let color = nes_color_code_to_rgb(color_code);
                            let point = Point { x, y };
                            video.draw_point(point, color);
                        }

                        yield;
                    }
                }
            }

            unreachable!();
        }
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
