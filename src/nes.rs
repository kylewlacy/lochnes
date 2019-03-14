use std::u8;
use std::fmt;
use std::cell::Cell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use bitflags::bitflags;
use enum_kinds::EnumKind;
use crate::rom::Rom;

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
            0x4014 => {
                self.copy_oam_dma(value);
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

    fn run_cpu<'a>(&'a self)
        -> impl Generator<Yield = CpuStep, Return = !> + 'a
    {
        move || loop {
            let nmi = self.cpu.nmi.get();
            if nmi {
                println!("=== NMI ===");
                self.cpu.nmi.set(false);

                self.push_u16(self.cpu.pc.get());
                self.push_u8(self.cpu.p.get().bits);

                let nmi_vector = self.read_u16(0xFFFA);
                self.cpu.pc.set(nmi_vector);
            }

            let pc = self.cpu.pc.get();
            let next_pc;

            let opcode = self.read_u8(pc);
            let opcode = Opcode::from_u8(opcode);

            let op;
            match opcode {
                Opcode::AdcZero => {
                    let a = self.cpu.a.get();
                    let p = self.cpu.p.get();
                    let c = if p.contains(CpuFlags::C) { 1 } else { 0 };

                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);

                    let result = a as u16 + c as u16 + value as u16;
                    let out = result as u8;

                    // TODO: Refactor!
                    let signed_result = result as i8;
                    let signed_out = out as i8;
                    let is_sign_correct =
                        (signed_result >= 0 && signed_out >= 0)
                        || (signed_result < 0 && signed_out < 0);

                    self.cpu.a.set(out);
                    self.cpu.set_flags(CpuFlags::C, result > u8::MAX as u16);
                    self.cpu.set_flags(CpuFlags::Z, result == 0);
                    self.cpu.set_flags(CpuFlags::V, !is_sign_correct);
                    self.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AdcZero { zero_page };
                }
                Opcode::AdcImm => {
                    let a = self.cpu.a.get();
                    let p = self.cpu.p.get();
                    let c = if p.contains(CpuFlags::C) { 1 } else { 0 };

                    let value = self.read_u8(pc + 1);

                    let result = a as u16 + c as u16 + value as u16;
                    let out = result as u8;

                    // TODO: Refactor!
                    let signed_result = result as i8;
                    let signed_out = out as i8;
                    let is_sign_correct =
                        (signed_result >= 0 && signed_out >= 0)
                        || (signed_result < 0 && signed_out < 0);

                    self.cpu.a.set(out);
                    self.cpu.set_flags(CpuFlags::C, result > u8::MAX as u16);
                    self.cpu.set_flags(CpuFlags::Z, result == 0);
                    self.cpu.set_flags(CpuFlags::V, !is_sign_correct);
                    self.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AdcImm { value };
                }
                Opcode::AndImm => {
                    let a = self.cpu.a.get();
                    let value = self.read_u8(pc + 1);
                    let a = a & value;
                    self.cpu.a.set(a);

                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AndImm { value };
                }
                Opcode::AndZero => {
                    let a = self.cpu.a.get();
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);
                    let a = a & value;
                    self.cpu.a.set(a);

                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::AndZero { zero_page };
                }
                Opcode::Beq => {
                    let addr_offset = self.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if self.cpu.contains_flags(CpuFlags::Z) {
                        // TODO: Handle offset past page! With that, `i8` shouldn't
                        // be necessary
                        next_pc = (pc_after as i16 + addr_offset as i16) as u16;
                    }
                    else {
                        next_pc = pc_after;
                    }
                    op = Op::Beq { addr_offset };
                }
                Opcode::Bne => {
                    let addr_offset = self.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if self.cpu.contains_flags(CpuFlags::Z) {
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
                    let addr_offset = self.read_i8(pc + 1);

                    let pc_after = pc + 2;
                    if self.cpu.contains_flags(CpuFlags::N) {
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
                    self.cpu.set_flags(CpuFlags::C, false);

                    next_pc = pc + 1;
                    op = Op::Clc;
                }
                Opcode::Cld => {
                    self.cpu.set_flags(CpuFlags::D, false);
                    next_pc = pc + 1;
                    op = Op::Cld;
                }
                Opcode::CmpImm => {
                    let value = self.read_u8(pc + 1);
                    let a = self.cpu.a.get();
                    let result = a.wrapping_sub(value);

                    self.cpu.set_flags(CpuFlags::C, a >= value);
                    self.cpu.set_flags(CpuFlags::Z, a == value);
                    self.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::CmpImm { value };
                }
                Opcode::DecZero => {
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);
                    let value = value.wrapping_sub(1);
                    self.write_u8(addr, value);

                    self.cpu.set_flags(CpuFlags::Z, value == 0);
                    self.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::DecZero { zero_page };
                }
                Opcode::Dex => {
                    let x = self.cpu.x.get().wrapping_sub(1);
                    self.cpu.set_flags(CpuFlags::Z, x == 0);
                    self.cpu.set_flags(CpuFlags::N, (x & 0b_1000_0000) != 0);
                    self.cpu.x.set(x);

                    next_pc = pc + 1;
                    op = Op::Dex;
                }
                Opcode::Dey => {
                    let y = self.cpu.y.get().wrapping_sub(1);
                    self.cpu.set_flags(CpuFlags::Z, y == 0);
                    self.cpu.set_flags(CpuFlags::N, (y & 0b_1000_0000) != 0);
                    self.cpu.y.set(y);

                    next_pc = pc + 1;
                    op = Op::Dey;
                }
                Opcode::EorImm => {
                    let value = self.read_u8(pc + 1);
                    let a = self.cpu.a.get() ^ value;
                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);
                    self.cpu.a.set(a);

                    next_pc = pc + 2;
                    op = Op::EorImm { value };
                }
                Opcode::EorZero => {
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);
                    let a = self.cpu.a.get() ^ value;
                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);
                    self.cpu.a.set(a);

                    next_pc = pc + 2;
                    op = Op::EorZero { zero_page };
                }
                Opcode::Iny => {
                    let y = self.cpu.y.get().wrapping_add(1);
                    self.cpu.set_flags(CpuFlags::Z, y == 0);
                    self.cpu.set_flags(CpuFlags::N, (y & 0b_1000_0000) != 0);
                    self.cpu.y.set(y);

                    next_pc = pc + 1;
                    op = Op::Iny;
                }
                Opcode::JmpAbs => {
                    let addr = self.read_u16(pc + 1);

                    next_pc = addr;
                    op = Op::JmpAbs { addr };
                }
                Opcode::Jsr => {
                    let addr = self.read_u16(pc + 1);
                    let ret_pc = pc.wrapping_add(3);
                    let push_pc = ret_pc.wrapping_sub(1);

                    self.push_u16(push_pc);

                    next_pc = addr;
                    op = Op::Jsr { addr };
                }
                Opcode::LdaAbs => {
                    // TODO: Flags!
                    let addr = self.read_u16(pc + 1);
                    let value = self.read_u8(addr);
                    self.cpu.a.set(value);
                    next_pc = pc + 3;
                    op = Op::LdaAbs { addr };
                }
                Opcode::LdaAbsX => {
                    // TODO: Flags!
                    let x = self.cpu.x.get();
                    let addr_base = self.read_u16(pc + 1);
                    let addr = addr_base.wrapping_add(x as u16);
                    let value = self.read_u8(addr);
                    self.cpu.a.set(value);
                    next_pc = pc + 3;
                    op = Op::LdaAbsX { addr_base };
                }
                Opcode::LdaImm => {
                    let value = self.read_u8(pc + 1);
                    self.cpu.a.set(value);
                    next_pc = pc + 2;
                    op = Op::LdaImm { value };
                }
                Opcode::LdaIndY => {
                    let y = self.cpu.y.get();
                    let target_addr_base = self.read_u8(pc + 1);

                    // TODO: Is this right? Does the target address
                    // wrap around the zero page?
                    let addr_base = self.read_u16(target_addr_base as u16);
                    let addr = addr_base.wrapping_add(y as u16);
                    let value = self.read_u8(addr);

                    self.cpu.a.set(value);

                    next_pc = pc + 2;
                    op = Op::LdaIndY { target_addr_base };
                }
                Opcode::LdaZero => {
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);
                    self.cpu.a.set(value);
                    next_pc = pc + 2;
                    op = Op::LdaZero { zero_page };
                }
                Opcode::LdxAbs => {
                    let addr = self.read_u16(pc + 1);
                    let value = self.read_u8(addr);
                    self.cpu.x.set(value);
                    next_pc = pc + 3;
                    op = Op::LdxAbs { addr };
                }
                Opcode::LdxImm => {
                    let value = self.read_u8(pc + 1);
                    self.cpu.x.set(value);
                    next_pc = pc + 2;
                    op = Op::LdxImm { value };
                }
                Opcode::LdxZero => {
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);
                    self.cpu.x.set(value);
                    next_pc = pc + 2;
                    op = Op::LdxZero { zero_page };
                }
                Opcode::LdyImm => {
                    let value = self.read_u8(pc + 1);
                    self.cpu.y.set(value);
                    next_pc = pc + 2;
                    op = Op::LdyImm { value };
                }
                Opcode::LsrA => {
                    let a = self.cpu.a.get();
                    let carry = (a & 0b_0000_0001) != 0;

                    let result = a >> 1;
                    self.cpu.a.set(result);
                    self.cpu.set_flags(CpuFlags::C, carry);
                    self.cpu.set_flags(CpuFlags::Z, result == 0);
                    self.cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::LsrA;
                }
                Opcode::Pha => {
                    let a = self.cpu.a.get();
                    self.push_u8(a);
                    next_pc = pc + 1;
                    op = Op::Pha;
                }
                Opcode::Pla => {
                    let a = self.pull_u8();
                    self.cpu.a.set(a);

                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::Pla;
                }
                Opcode::RorZero => {
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    let value = self.read_u8(addr);

                    let c = (value & 0b_0000_0001) != 0;
                    let new_value = value >> 1;
                    self.write_u8(addr, new_value);

                    self.cpu.set_flags(CpuFlags::C, c);
                    self.cpu.set_flags(CpuFlags::Z, value == 0);
                    self.cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);

                    next_pc = pc + 2;
                    op = Op::RorZero { zero_page };
                }
                Opcode::Rts => {
                    let push_pc = self.pull_u16();
                    let ret_pc = push_pc.wrapping_add(1);

                    next_pc = ret_pc;
                    op = Op::Rts;
                }
                Opcode::Sei => {
                    self.cpu.set_flags(CpuFlags::I, true);
                    next_pc = pc + 1;
                    op = Op::Sei;
                }
                Opcode::StaAbs => {
                    let a = self.cpu.a.get();
                    let addr = self.read_u16(pc + 1);
                    self.write_u8(addr, a);
                    next_pc = pc + 3;
                    op = Op::StaAbs { addr };
                }
                Opcode::StaZero => {
                    let a = self.cpu.a.get();
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    self.write_u8(addr, a);
                    next_pc = pc + 2;
                    op = Op::StaZero { zero_page };
                }
                Opcode::StaIndY => {
                    let a = self.cpu.a.get();
                    let y = self.cpu.y.get();
                    let target_addr_base = self.read_u8(pc + 1);

                    // TODO: Is this right? Does the target address
                    // wrap around the zero page?
                    let addr_base = self.read_u16(target_addr_base as u16);
                    let addr = addr_base.wrapping_add(y as u16);
                    self.write_u8(addr, a);

                    next_pc = pc + 2;
                    op = Op::StaIndY { target_addr_base };
                }
                Opcode::StyZero => {
                    let y = self.cpu.y.get();
                    let zero_page = self.read_u8(pc + 1);
                    let addr = zero_page as u16;
                    self.write_u8(addr, y);
                    next_pc = pc + 2;
                    op = Op::StyZero { zero_page };
                }
                Opcode::Tax => {
                    let a = self.cpu.a.get();
                    self.cpu.x.set(a);
                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::Tax;
                }
                Opcode::Tay => {
                    let a = self.cpu.a.get();
                    self.cpu.y.set(a);
                    self.cpu.set_flags(CpuFlags::Z, a == 0);
                    self.cpu.set_flags(CpuFlags::N, (a & 0b_1000_0000) != 0);

                    next_pc = pc + 1;
                    op = Op::Tay;
                }
                Opcode::Txa => {
                    let x = self.cpu.x.get();
                    self.cpu.a.set(x);
                    next_pc = pc + 1;
                    op = Op::Txa;
                }
                Opcode::Txs => {
                    let x = self.cpu.x.get();
                    self.cpu.s.set(x);
                    next_pc = pc + 1;
                    op = Op::Txs;
                }
                Opcode::Tya => {
                    let y = self.cpu.y.get();
                    self.cpu.a.set(y);
                    next_pc = pc + 1;
                    op = Op::Tya;
                }
            }

            self.cpu.pc.set(next_pc);

            debug_assert_eq!(Opcode::from(&op), opcode);

            yield CpuStep { pc, op };
        }
    }

    fn run_ppu<'a>(&'a self) -> impl Generator<Yield = (), Return = !> + 'a {
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
                            let _ = self.ppu.status.update(|mut status| {
                                status.set(PpuStatusFlags::VBLANK_STARTED, true);
                                status
                            });
                            self.cpu.nmi.set(true);
                        }

                        yield;
                    }
                }
            }

            unreachable!();
        }
    }

    pub fn run<'a>(&'a self)
        -> impl Generator<Yield = CpuStep, Return = !> + 'a
    {
        let mut run_cpu = self.run_cpu();

        let mut run_ppu = self.run_ppu();

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
    AndImm { value: u8 },
    AndZero { zero_page: u8 },
    Beq { addr_offset: i8 },
    Bne { addr_offset: i8 },
    Bpl { addr_offset: i8 },
    Clc,
    Cld,
    CmpImm { value: u8 },
    DecZero { zero_page: u8 },
    Dex,
    Dey,
    EorImm { value: u8 },
    EorZero { zero_page: u8 },
    Iny,
    JmpAbs { addr: u16 },
    Jsr { addr: u16 },
    LdaAbs { addr: u16 },
    LdaAbsX { addr_base: u16 },
    LdaImm { value: u8 },
    LdaZero { zero_page: u8 },
    LdaIndY { target_addr_base: u8 },
    LdxAbs { addr: u16 },
    LdxImm { value: u8 },
    LdxZero { zero_page: u8 },
    LdyImm { value: u8 },
    LsrA,
    Pha,
    Pla,
    RorZero { zero_page: u8 },
    Rts,
    Sei,
    StaAbs { addr: u16 },
    StaZero { zero_page: u8 },
    StaIndY { target_addr_base: u8 },
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
            0x10 => Opcode::Bpl,
            0x18 => Opcode::Clc,
            0x20 => Opcode::Jsr,
            0x25 => Opcode::AndZero,
            0x29 => Opcode::AndImm,
            0x45 => Opcode::EorZero,
            0x48 => Opcode::Pha,
            0x49 => Opcode::EorImm,
            0x4A => Opcode::LsrA,
            0x4C => Opcode::JmpAbs,
            0x60 => Opcode::Rts,
            0x65 => Opcode::AdcZero,
            0x66 => Opcode::RorZero,
            0x68 => Opcode::Pla,
            0x69 => Opcode::AdcImm,
            0x78 => Opcode::Sei,
            0x8A => Opcode::Txa,
            0x84 => Opcode::StyZero,
            0x85 => Opcode::StaZero,
            0x88 => Opcode::Dey,
            0x8D => Opcode::StaAbs,
            0x91 => Opcode::StaIndY,
            0x98 => Opcode::Tya,
            0x9A => Opcode::Txs,
            0xA0 => Opcode::LdyImm,
            0xA2 => Opcode::LdxImm,
            0xA5 => Opcode::LdaZero,
            0xA6 => Opcode::LdxZero,
            0xA8 => Opcode::Tay,
            0xA9 => Opcode::LdaImm,
            0xAA => Opcode::Tax,
            0xAD => Opcode::LdaAbs,
            0xAE => Opcode::LdxAbs,
            0xB1 => Opcode::LdaIndY,
            0xBD => Opcode::LdaAbsX,
            0xC6 => Opcode::DecZero,
            0xC8 => Opcode::Iny,
            0xC9 => Opcode::CmpImm,
            0xCA => Opcode::Dex,
            0xD0 => Opcode::Bne,
            0xD8 => Opcode::Cld,
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
            Opcode::AdcImm | Opcode::AdcZero => "ADC",
            Opcode::AndImm | Opcode::AndZero => "AND",
            Opcode::Beq => "BEQ",
            Opcode::Bne => "BNE",
            Opcode::Bpl => "BPL",
            Opcode::Clc => "CLC",
            Opcode::Cld => "CLD",
            Opcode::CmpImm => "CMP",
            Opcode::DecZero => "DEC",
            Opcode::Dex => "DEX",
            Opcode::Dey => "DEY",
            Opcode::EorImm | Opcode::EorZero => "EOR",
            Opcode::Iny => "INY",
            Opcode::JmpAbs => "JMP",
            Opcode::Jsr => "JSR",
            Opcode::LdaAbs
            | Opcode::LdaAbsX
            | Opcode::LdaImm
            | Opcode::LdaIndY
            | Opcode::LdaZero => "LDA",
            Opcode::LdxAbs
            | Opcode::LdxImm
            | Opcode::LdxZero => "LDX",
            Opcode::LdyImm => "LDY",
            Opcode::LsrA => "LSR",
            Opcode::Pha => "PHA",
            Opcode::Pla => "PLA",
            Opcode::RorZero => "ROR",
            Opcode::Rts => "RTS",
            Opcode::Sei => "SEI",
            Opcode::StaAbs | Opcode::StaZero | Opcode::StaIndY => "STA",
            Opcode::StyZero => "STY",
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
            | Op::Iny
            | Op::Pha
            | Op::Pla
            | Op::Rts
            | Op::Sei
            | Op::Tax
            | Op::Tay
            | Op::Txa
            | Op::Txs
            | Op::Tya => {
                write!(f, "{}", opcode)?;
            }
            Op::LsrA => {
                write!(f, "{} A", opcode)?;
            }
            Op::LdaAbs { addr }
            | Op::LdxAbs { addr }
            | Op::StaAbs { addr }
            | Op::JmpAbs { addr }
            | Op::Jsr { addr } => {
                write!(f, "{} ${:04X}", opcode, addr)?;
            }
            Op::LdaAbsX { addr_base } => {
                write!(f, "{} ${:04X},X", opcode, addr_base)?;
            }
            Op::AdcZero { zero_page }
            | Op::AndZero { zero_page }
            | Op::DecZero { zero_page }
            | Op::EorZero { zero_page }
            | Op::LdaZero { zero_page }
            | Op::LdxZero { zero_page }
            | Op::RorZero { zero_page }
            | Op::StaZero { zero_page }
            | Op::StyZero { zero_page }=> {
                write!(f, "{} ${:04X}", opcode, *zero_page as u16)?;
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
            | Op::LdyImm { value } => {
                write!(f, "{} #${:02X}", opcode, value)?;
            }
            Op::Beq { addr_offset }
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

    pattern_tables: Cell<[u8; 2 * 0x1000]>,
    nametables: Cell<[u8; 4 * 0x0400]>,
    oam: Cell<[u8; 0x0100]>,
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
            pattern_tables: Cell::new([0; 2 * 0x1000]),
            nametables: Cell::new([0; 4 * 0x0400]),
            oam: Cell::new([0; 0x0100]),
        }
    }

    fn pattern_tables(&self) -> &[Cell<u8>] {
        let pattern_tables: &Cell<[u8]> = &self.pattern_tables;
        pattern_tables.as_slice_of_cells()
    }

    fn nametables(&self) -> &[Cell<u8>] {
        let nametables: &Cell<[u8]> = &self.nametables;
        nametables.as_slice_of_cells()
    }

    fn set_ppuctrl(&self, value: u8) {
        self.ctrl.set(PpuCtrlFlags::from_bits_truncate(value));
    }

    fn set_ppumask(&self, value: u8) {
        self.mask.set(PpuMaskFlags::from_bits_truncate(value));
    }

    fn write_addr(&self, addr: u16, value: u8) {
        let pattern_tables = self.pattern_tables();
        let nametables = self.nametables();

        match addr {
            0x0000..=0x0FFF => {
                pattern_tables[addr as usize].set(value);
            }
            0x2000..=0x23FF => {
                let offset = addr as usize - 0x2000;
                nametables[offset].set(value);
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
