use std::u8;
use std::fmt;
use std::cell::Cell;
use std::ops::Generator;
use bitflags::bitflags;
use enum_kinds::EnumKind;
use crate::nes::Nes;

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
    pub fn new() -> Self {
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

    fn pc_inc(&self) -> u16 {
        let pc = self.pc.get();

        // TODO: Should this be using `wrapping_add`?
        self.pc.set(pc.wrapping_add(1));
        pc
    }

    fn pc_fetch(nes: &Nes) -> u8 {
        let pc = nes.cpu.pc.get();
        nes.read_u8(pc)
    }

    fn pc_fetch_inc(nes: &Nes) -> u8 {
        let pc = nes.cpu.pc_inc();
        nes.read_u8(pc)
    }

    fn stack_addr(&self) -> u16 {
        0x0100 | self.s.get() as u16
    }

    fn inc_s(&self) {
        self.s.update(|s| s.wrapping_add(1));
    }

    fn dec_s(&self) {
        self.s.update(|s| s.wrapping_sub(1));
    }

    pub fn run<'a>(nes: &'a Nes)
        -> impl Generator<Yield = CpuStep, Return = !> + 'a
    {
        move || loop {
            // TODO: Does this properly account for CPU cycles for handling NMI?
            let nmi = nes.cpu.nmi.get();
            if nmi {
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
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_read(nes, AdcOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::AdcAbs { addr };
                }
                Opcode::AdcImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, AdcOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::AdcImm { value };
                }
                Opcode::AdcZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, AdcOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::AdcZero { zero_page };
                }
                Opcode::AndImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, AndOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::AndImm { value };
                }
                Opcode::AndZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, AndOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::AndZero { zero_page };
                }
                Opcode::AndZeroX => {
                    let ZeroPageXArg { zero_page_base } = yield_all! {
                        zero_page_x_read(nes, AndOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::AndZeroX { zero_page_base };
                }
                Opcode::AslA => {
                    let () = yield_all! {
                        accum_read_modify_write(nes, AslOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::AslA;
                }
                Opcode::AslZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read_modify_write(nes, AslOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::AslZero { zero_page };
                }
                Opcode::Bcc => {
                    let BranchArg { addr_offset } = yield_all! {
                        branch(&nes, BccOperation)
                    };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Bcc { addr_offset };
                }
                Opcode::Bcs => {
                    let BranchArg { addr_offset } = yield_all! {
                        branch(&nes, BcsOperation)
                    };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Bcs { addr_offset };
                }
                Opcode::Beq => {
                    let BranchArg { addr_offset } = yield_all! {
                        branch(&nes, BeqOperation)
                    };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Beq { addr_offset };
                }
                Opcode::Bmi => {
                    let BranchArg { addr_offset } = yield_all! {
                        branch(&nes, BmiOperation)
                    };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Bmi { addr_offset };
                }
                Opcode::Bne => {
                    let BranchArg { addr_offset } = yield_all! {
                        branch(&nes, BneOperation)
                    };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Bne { addr_offset };
                }
                Opcode::Bpl => {
                    let BranchArg { addr_offset } = yield_all! {
                        branch(&nes, BplOperation)
                    };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Bpl { addr_offset };
                }
                Opcode::Clc => {
                    let () = yield_all! {
                        implied(nes, ClcOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Clc;
                }
                Opcode::Cld => {
                    let () = yield_all! {
                        implied(nes, CldOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Cld;
                }
                Opcode::CmpImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, CmpOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::CmpImm { value };
                }
                Opcode::DecAbs => {
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_read_modify_write(nes, DecOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::DecAbs { addr };
                }
                Opcode::DecZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read_modify_write(nes, DecOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::DecZero { zero_page };
                }
                Opcode::DecZeroX => {
                    let ZeroPageXArg { zero_page_base } = yield_all! {
                        zero_page_x_read_modify_write(nes, DecOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::DecZeroX { zero_page_base };
                }
                Opcode::Dex => {
                    let () = yield_all! {
                        implied(nes, DexOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Dex;
                }
                Opcode::Dey => {
                    let () = yield_all! {
                        implied(nes, DeyOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Dey;
                }
                Opcode::EorImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, EorOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::EorImm { value };
                }
                Opcode::EorZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, EorOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::EorZero { zero_page };
                }
                Opcode::IncZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read_modify_write(nes, IncOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::IncZero { zero_page };
                }
                Opcode::Inx => {
                    let () = yield_all! {
                        implied(nes, InxOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Inx;
                }
                Opcode::Iny => {
                    let () = yield_all! {
                        implied(nes, InyOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Iny;
                }
                Opcode::JmpAbs => {
                    let AbsoluteArg { addr } = yield_all! { absolute_jmp(nes) };
                    next_pc = addr;
                    op = Op::JmpAbs { addr };
                }
                Opcode::Jsr => {
                    let JsrArg { addr } = yield_all! { jsr(nes) };
                    next_pc = addr;
                    op = Op::Jsr { addr };
                }
                Opcode::LdaAbs => {
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_read(nes, LdaOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::LdaAbs { addr };
                }
                Opcode::LdaAbsX => {
                    let AbsoluteXArg { addr_base } = yield_all! {
                        absolute_x_read(nes, LdaOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::LdaAbsX { addr_base };
                }
                Opcode::LdaAbsY => {
                    let AbsoluteYArg { addr_base } = yield_all! {
                        absolute_y_read(nes, LdaOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::LdaAbsY { addr_base };
                }
                Opcode::LdaImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, LdaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdaImm { value };
                }
                Opcode::LdaIndY => {
                    let IndirectYArg { target_addr_base } = yield_all! {
                        indirect_y_read(nes, LdaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdaIndY { target_addr_base };
                }
                Opcode::LdaZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, LdaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdaZero { zero_page };
                }
                Opcode::LdaZeroX => {
                    let ZeroPageXArg { zero_page_base } = yield_all! {
                        zero_page_x_read(nes, LdaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdaZeroX { zero_page_base };
                }
                Opcode::LdxAbs => {
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_read(nes, LdxOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::LdxAbs { addr };
                }
                Opcode::LdxImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, LdxOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdxImm { value };
                }
                Opcode::LdxZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, LdxOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdxZero { zero_page };
                }
                Opcode::LdyImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, LdyOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdyImm { value };
                }
                Opcode::LdyZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, LdyOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdyZero { zero_page };
                }
                Opcode::LdyZeroX => {
                    let ZeroPageXArg { zero_page_base } = yield_all! {
                        zero_page_x_read(nes, LdyOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LdyZeroX { zero_page_base };
                }
                Opcode::LsrA => {
                    let () = yield_all! {
                        accum_read_modify_write(nes, LsrOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::LsrA;
                }
                Opcode::LsrZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read_modify_write(nes, LsrOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::LsrZero { zero_page };
                }
                Opcode::OraImm => {
                    let ImmArg { value } = yield_all! {
                        imm_read(nes, OraOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::OraImm { value };
                }
                Opcode::OraZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read(nes, OraOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::OraZero { zero_page };
                }
                Opcode::Pha => {
                    let () = yield_all! {
                        stack_push(nes, PhaOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Pha;
                }
                Opcode::Pla => {
                    let () = yield_all! {
                        stack_pull(nes, PlaOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Pla;
                }
                Opcode::RolA => {
                    let () = yield_all! {
                        accum_read_modify_write(nes, RolOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::RolA;
                }
                Opcode::RorA => {
                    let () = yield_all! {
                        accum_read_modify_write(nes, RorOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::RorA;
                }
                Opcode::RorZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_read_modify_write(nes, RorOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::RorZero { zero_page };
                }
                Opcode::Rti => {
                    let () = yield_all! { rti(nes) };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Rti;
                }
                Opcode::Rts => {
                    let () = yield_all! { rts(nes) };
                    next_pc = nes.cpu.pc.get();
                    op = Op::Rts;
                }
                Opcode::Sec => {
                    let () = yield_all! {
                        implied(nes, SecOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Sec;
                }
                Opcode::Sei => {
                    let () = yield_all! {
                        implied(nes, SeiOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Sei;
                }
                Opcode::StaAbs => {
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_write(nes, StaOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::StaAbs { addr };
                }
                Opcode::StaAbsX => {
                    let AbsoluteXArg { addr_base } = yield_all! {
                        absolute_x_write(nes, StaOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::StaAbsX { addr_base };
                }
                Opcode::StaIndY => {
                    let IndirectYArg { target_addr_base } = yield_all! {
                        indirect_y_write(nes, StaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::StaIndY { target_addr_base };
                }
                Opcode::StaZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_write(nes, StaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::StaZero { zero_page };
                }
                Opcode::StaZeroX => {
                    let ZeroPageXArg { zero_page_base } = yield_all! {
                        zero_page_x_write(nes, StaOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::StaZeroX { zero_page_base };
                }
                Opcode::StxAbs => {
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_write(nes, StxOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::StxAbs { addr };
                }
                Opcode::StxZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_write(nes, StxOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::StxZero { zero_page };
                }
                Opcode::StyAbs => {
                    let AbsoluteArg { addr } = yield_all! {
                        absolute_write(nes, StyOperation)
                    };
                    next_pc = pc + 3;
                    op = Op::StyAbs { addr };
                }
                Opcode::StyZero => {
                    let ZeroPageArg { zero_page } = yield_all! {
                        zero_page_write(nes, StyOperation)
                    };
                    next_pc = pc + 2;
                    op = Op::StyZero { zero_page };
                }
                Opcode::Tax => {
                    let () = yield_all! {
                        implied(nes, TaxOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Tax;
                }
                Opcode::Tay => {
                    let () = yield_all! {
                        implied(nes, TayOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Tay;
                }
                Opcode::Txa => {
                    let () = yield_all! {
                        implied(nes, TxaOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Txa;
                }
                Opcode::Txs => {
                    let () = yield_all! {
                        implied(nes, TxsOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Txs;
                }
                Opcode::Tya => {
                    let () = yield_all! {
                        implied(nes, TyaOperation)
                    };
                    next_pc = pc + 1;
                    op = Op::Tya;
                }
            }

            nes.cpu.pc.set(next_pc);

            debug_assert_eq!(Opcode::from(&op), opcode);

            yield CpuStep::Op(CpuStepOp { pc, op });
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

pub enum CpuStep {
    Cycle,
    Op(CpuStepOp),
}

pub struct CpuStepOp {
    pub pc: u16,
    pub op: Op,
}



trait ImpliedOperation {
    fn operate(&self, cpu: &Cpu);
}

trait ReadOperation {
    fn read(&self, cpu: &Cpu, value: u8);
}

trait ReadModifyWriteOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8;
}

trait WriteOperation {
    fn write(&self, cpu: &Cpu) -> u8;
}

trait BranchOperation {
    fn branch(&self, cpu: &Cpu) -> bool;
}

trait StackPushOperation {
    fn push(&self, cpu: &Cpu) -> u8;
}

trait StackPullOperation {
    fn pull(&self, cpu: &Cpu, value: u8);
}

fn implied<'a>(
    nes: &'a Nes,
    op: impl ImpliedOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = ()> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        op.operate(&nes.cpu);
        yield CpuStep::Cycle;
    }
}

fn accum_read_modify_write<'a>(
    nes: &'a Nes,
    op: impl ReadModifyWriteOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = ()> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);

        let value = nes.cpu.a.get();
        let new_value = op.modify(&nes.cpu, value);
        nes.cpu.a.set(new_value);
        yield CpuStep::Cycle;
    }
}

struct ImmArg {
    value: u8
}

fn imm_read<'a>(
    nes: &'a Nes,
    op: impl ReadOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = ImmArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let value = Cpu::pc_fetch_inc(nes);
        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        ImmArg { value }
    }
}

struct ZeroPageArg { zero_page: u8 }

fn zero_page_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = ZeroPageArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr = zero_page as u16;
        let value = nes.read_u8(addr);

        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        ZeroPageArg { zero_page }
    }
}

fn zero_page_read_modify_write<'a>(
    nes: &'a Nes,
    op: impl ReadModifyWriteOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = ZeroPageArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr = zero_page as u16;
        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        let new_value = op.modify(&nes.cpu, value);
        yield CpuStep::Cycle;

        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

        ZeroPageArg { zero_page }
    }
}

fn zero_page_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = ZeroPageArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr = zero_page as u16;

        let value = op.write(&nes.cpu);
        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        ZeroPageArg { zero_page }
    }
}

struct ZeroPageXArg { zero_page_base: u8 }

fn zero_page_x_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = ZeroPageXArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(zero_page_base as u16);
        let addr = zero_page_base.wrapping_add(nes.cpu.x.get()) as u16;
        yield CpuStep::Cycle;

        let value = nes.read_u8(addr);
        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        ZeroPageXArg { zero_page_base }
    }
}

fn zero_page_x_read_modify_write<'a>(
    nes: &'a Nes,
    op: impl ReadModifyWriteOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = ZeroPageXArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(zero_page_base as u16);
        let addr = zero_page_base.wrapping_add(nes.cpu.x.get()) as u16;
        yield CpuStep::Cycle;

        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        let new_value = op.modify(&nes.cpu, value);
        yield CpuStep::Cycle;

        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

        ZeroPageXArg { zero_page_base }
    }
}

fn zero_page_x_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = ZeroPageXArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(zero_page_base as u16);
        let addr = zero_page_base.wrapping_add(nes.cpu.x.get()) as u16;
        yield CpuStep::Cycle;

        let value = op.write(&nes.cpu);
        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        ZeroPageXArg { zero_page_base }
    }
}

struct AbsoluteArg { addr: u16 }

fn absolute_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr = u16_from(addr_lo, addr_hi);
        let value = nes.read_u8(addr);

        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        AbsoluteArg { addr }
    }
}

fn absolute_read_modify_write<'a>(
    nes: &'a Nes,
    op: impl ReadModifyWriteOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr = u16_from(addr_lo, addr_hi);
        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        let new_value = op.modify(&nes.cpu, value);
        yield CpuStep::Cycle;

        nes.write_u8(addr, new_value);

        AbsoluteArg { addr }
    }
}

fn absolute_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr = u16_from(addr_lo, addr_hi);
        let value = op.write(&nes.cpu);

        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        AbsoluteArg { addr }
    }
}

fn absolute_jmp<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let pc_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let pc_hi = Cpu::pc_fetch(nes);
        let addr = u16_from(pc_lo, pc_hi);
        nes.cpu.pc.set(addr);
        yield CpuStep::Cycle;

        AbsoluteArg { addr }
    }
}

struct AbsoluteXArg { addr_base: u16 }

fn absolute_x_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteXArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch_inc(nes);
        let addr_base = u16_from(addr_lo, addr_hi);
        let x = nes.cpu.x.get();
        let addr_lo_x = addr_lo.wrapping_add(x);
        yield CpuStep::Cycle;

        let addr_unfixed = u16_from(addr_lo_x, addr_hi);
        let value_unfixed = nes.read_u8(addr_unfixed);

        // Speculatively execute the operation based on the
        // incomplete address calculation
        op.read(&nes.cpu, value_unfixed);
        yield CpuStep::Cycle;

        // Calculate the actual address to use
        let addr = addr_base.wrapping_add(x as u16);
        if addr != addr_unfixed {
            // Re-run the operation if the original calculation
            // was incorrect (i.e. a page boundary was crossed)
            let value = nes.read_u8(addr);
            op.read(&nes.cpu, value);
            yield CpuStep::Cycle;
        }

        AbsoluteXArg { addr_base }
    }
}

fn absolute_x_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteXArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch_inc(nes);
        let addr_base = u16_from(addr_lo, addr_hi);
        let x = nes.cpu.x.get();
        let addr_lo_x = addr_lo.wrapping_add(x);
        yield CpuStep::Cycle;

        let addr_unfixed = u16_from(addr_lo_x, addr_hi);
        let _garbage = nes.read_u8(addr_unfixed);
        yield CpuStep::Cycle;

        let addr = addr_base.wrapping_add(x as u16);
        let new_value = op.write(&nes.cpu);
        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

        AbsoluteXArg { addr_base }
    }
}

struct AbsoluteYArg { addr_base: u16 }

fn absolute_y_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = AbsoluteYArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch_inc(nes);
        let addr_base = u16_from(addr_lo, addr_hi);
        let y = nes.cpu.y.get();
        let addr_lo_y = addr_lo.wrapping_add(y);
        yield CpuStep::Cycle;

        let addr_unfixed = u16_from(addr_lo_y, addr_hi);
        let value_unfixed = nes.read_u8(addr_unfixed);

        // Speculatively execute the operation based on the
        // incomplete address calculation
        op.read(&nes.cpu, value_unfixed);
        yield CpuStep::Cycle;

        // Calculate the actual address to use
        let addr = addr_base.wrapping_add(y as u16);
        if addr != addr_unfixed {
            // Re-run the operation if the original calculation
            // was incorrect (i.e. a page boundary was crossed)
            let value = nes.read_u8(addr);
            op.read(&nes.cpu, value);
            yield CpuStep::Cycle;
        }

        AbsoluteYArg { addr_base }
    }
}

struct IndirectYArg { target_addr_base: u8 }

fn indirect_y_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = IndirectYArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(&nes);
        yield CpuStep::Cycle;

        let target_addr_base = Cpu::pc_fetch_inc(&nes);
        let target_addr_base_lo = target_addr_base as u16;
        let target_addr_base_hi = target_addr_base_lo.wrapping_add(1);
        yield CpuStep::Cycle;

        let addr_base_lo = nes.read_u8(target_addr_base_lo);
        yield CpuStep::Cycle;

        let addr_base_hi = nes.read_u8(target_addr_base_hi);
        let y = nes.cpu.y.get();

        let addr_unfixed = u16_from(addr_base_lo.wrapping_add(y), addr_base_hi);
        yield CpuStep::Cycle;

        // Speculatively execute the operation based on the
        // incomplete address calculation
        let value_unfixed = nes.read_u8(addr_unfixed);
        op.read(&nes.cpu, value_unfixed);
        yield CpuStep::Cycle;

        let addr_base = u16_from(addr_base_lo, addr_base_hi);
        let addr = addr_base.wrapping_add(y as u16);
        if addr != addr_unfixed {
            // Re-run the operation if the original calculation
            // was incorrect (i.e. a page boundary was crossed)
            let value = nes.read_u8(addr);
            op.read(&nes.cpu, value);
            yield CpuStep::Cycle;
        }

        IndirectYArg { target_addr_base }
    }
}

fn indirect_y_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = IndirectYArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(&nes);
        yield CpuStep::Cycle;

        let target_addr_base = Cpu::pc_fetch_inc(&nes);
        let target_addr_base_lo = target_addr_base as u16;
        let target_addr_base_hi = target_addr_base_lo.wrapping_add(1);
        yield CpuStep::Cycle;

        let addr_base_lo = nes.read_u8(target_addr_base_lo);
        yield CpuStep::Cycle;

        let addr_base_hi = nes.read_u8(target_addr_base_hi);
        let y = nes.cpu.y.get();

        let addr_unfixed = u16_from(addr_base_lo.wrapping_add(y), addr_base_hi);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(addr_unfixed);
        yield CpuStep::Cycle;

        let addr_base = u16_from(addr_base_lo, addr_base_hi);
        let addr = addr_base.wrapping_add(y as u16);

        let value = op.write(&nes.cpu);

        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        return IndirectYArg { target_addr_base };
    }
}

struct BranchArg { addr_offset: i8 }

fn branch<'a>(nes: &'a Nes, op: impl BranchOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = BranchArg> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(&nes);
        yield CpuStep::Cycle;

        let addr_offset = Cpu::pc_fetch_inc(&nes) as i8;
        yield CpuStep::Cycle;

        if op.branch(&nes.cpu) {
            let pc = nes.cpu.pc.get();
            let pc_hi = (pc >> 8) as u8;
            let pc_lo = (pc & 0x00FF) as u8;

            let pc_lo_offset = pc_lo.wrapping_add(addr_offset as u8);
            let unfixed_addr = u16_from(pc_hi, pc_lo_offset);

            let addr = pc.wrapping_add(addr_offset as u16);
            nes.cpu.pc.set(addr);

            if addr != unfixed_addr {
                // yield CpuStep::Cycle;
            }

            yield CpuStep::Cycle;
        }

        BranchArg { addr_offset }
    }
}

fn stack_push<'a>(
    nes: &'a Nes,
    op: impl StackPushOperation + 'a,
)
    -> impl Generator<Yield = CpuStep, Return = ()> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        yield CpuStep::Cycle;

        let value = op.push(&nes.cpu);
        nes.write_u8(nes.cpu.stack_addr(), value);
        nes.cpu.dec_s();
        yield CpuStep::Cycle;
    }
}

fn stack_pull<'a>(
    nes: &'a Nes,
    op: impl StackPullOperation + 'a,
)
    -> impl Generator<Yield = CpuStep, Return = ()> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        yield CpuStep::Cycle;

        nes.cpu.inc_s();
        yield CpuStep::Cycle;

        let value = nes.read_u8(nes.cpu.stack_addr());
        op.pull(&nes.cpu, value);
        yield CpuStep::Cycle;
    }
}

struct JsrArg { addr: u16 }

fn jsr<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = JsrArg> + 'a
{
    move || {
        let ret_pc = nes.cpu.pc.get().wrapping_add(3);
        let push_pc = ret_pc.wrapping_sub(1);
        let push_pc_hi = (push_pc >> 8) as u8;
        let push_pc_lo = (push_pc & 0x00FF) as u8;

        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let addr_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        // No-op
        yield CpuStep::Cycle;

        nes.write_u8(nes.cpu.stack_addr(), push_pc_hi);
        nes.cpu.dec_s();
        yield CpuStep::Cycle;


        nes.write_u8(nes.cpu.stack_addr(), push_pc_lo);
        nes.cpu.dec_s();
        yield CpuStep::Cycle;

        let addr_hi = Cpu::pc_fetch(nes);
        let addr = u16_from(addr_lo, addr_hi);
        nes.cpu.pc.set(addr);
        yield CpuStep::Cycle;

        JsrArg { addr }
    }
}

fn rti<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = ()> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        yield CpuStep::Cycle;

        nes.cpu.inc_s();
        yield CpuStep::Cycle;

        let p = nes.read_u8(nes.cpu.stack_addr());
        nes.cpu.p.set(CpuFlags::from_bits_truncate(p));
        nes.cpu.inc_s();
        yield CpuStep::Cycle;

        let pc_lo = nes.read_u8(nes.cpu.stack_addr());
        nes.cpu.inc_s();
        yield CpuStep::Cycle;

        let pc_hi = nes.read_u8(nes.cpu.stack_addr());
        nes.cpu.pc.set(u16_from(pc_lo, pc_hi));
        yield CpuStep::Cycle;
    }
}

fn rts<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = ()> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        yield CpuStep::Cycle;

        nes.cpu.inc_s();
        yield CpuStep::Cycle;

        let pull_pc_lo = nes.read_u8(nes.cpu.stack_addr());
        nes.cpu.inc_s();
        yield CpuStep::Cycle;

        let pull_pc_hi = nes.read_u8(nes.cpu.stack_addr());
        nes.cpu.pc.set(u16_from(pull_pc_lo, pull_pc_hi));
        yield CpuStep::Cycle;

        nes.cpu.pc_inc();
        yield CpuStep::Cycle;
    }
}



struct AdcOperation;
impl ReadOperation for AdcOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let a = cpu.a.get();
        let p = cpu.p.get();
        let c = if p.contains(CpuFlags::C) { 1 } else { 0 };

        let result = a as u16 + c as u16 + value as u16;
        let out = result as u8;

        // TODO: Refactor!
        let signed_result = result as i8;
        let signed_out = out as i8;
        let is_sign_correct =
            (signed_result >= 0 && signed_out >= 0)
            || (signed_result < 0 && signed_out < 0);

        cpu.a.set(out);
        cpu.set_flags(CpuFlags::C, result > u8::MAX as u16);
        cpu.set_flags(CpuFlags::Z, result == 0);
        cpu.set_flags(CpuFlags::V, !is_sign_correct);
        cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);
    }
}

struct AndOperation;
impl ReadOperation for AndOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let new_a = cpu.a.get() & value;

        cpu.a.set(new_a);
        cpu.set_flags(CpuFlags::Z, new_a == 0);
        cpu.set_flags(CpuFlags::N, (new_a & 0b_1000_0000) != 0);
    }
}

struct AslOperation;
impl ReadModifyWriteOperation for AslOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let c = (value & 0b_1000_0000) != 0;
        let new_value = value << 1;

        cpu.set_flags(CpuFlags::C, c);
        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }
}

struct BccOperation;
impl BranchOperation for BccOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::C)
    }
}

struct BcsOperation;
impl BranchOperation for BcsOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::C)
    }
}

struct BeqOperation;
impl BranchOperation for BeqOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::Z)
    }
}

struct BmiOperation;
impl BranchOperation for BmiOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::N)
    }
}

struct BneOperation;
impl BranchOperation for BneOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::Z)
    }
}

struct BplOperation;
impl BranchOperation for BplOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::N)
    }
}

struct ClcOperation;
impl ImpliedOperation for ClcOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::C, false);
    }
}

struct CldOperation;
impl ImpliedOperation for CldOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::D, false);
    }
}

struct CmpOperation;
impl ReadOperation for CmpOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let a = cpu.a.get();
        let result = a.wrapping_sub(value);

        cpu.set_flags(CpuFlags::C, a >= value);
        cpu.set_flags(CpuFlags::Z, a == value);
        cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);
    }
}

struct DecOperation;
impl ReadModifyWriteOperation for DecOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let new_value = value.wrapping_sub(1);

        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }
}

struct DexOperation;
impl ImpliedOperation for DexOperation {
    fn operate(&self, cpu: &Cpu) {
        let new_x = cpu.x.get().wrapping_sub(1);
        cpu.set_flags(CpuFlags::Z, new_x == 0);
        cpu.set_flags(CpuFlags::N, (new_x & 0b_1000_0000) != 0);
        cpu.x.set(new_x);
    }
}

struct DeyOperation;
impl ImpliedOperation for DeyOperation {
    fn operate(&self, cpu: &Cpu) {
        let new_y = cpu.y.get().wrapping_sub(1);
        cpu.set_flags(CpuFlags::Z, new_y == 0);
        cpu.set_flags(CpuFlags::N, (new_y & 0b_1000_0000) != 0);
        cpu.y.set(new_y);
    }
}

struct EorOperation;
impl ReadOperation for EorOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let new_a = cpu.a.get() ^ value;

        cpu.a.set(new_a);
        cpu.set_flags(CpuFlags::Z, new_a == 0);
        cpu.set_flags(CpuFlags::N, (new_a & 0b_1000_0000) != 0);
    }
}

struct IncOperation;
impl ReadModifyWriteOperation for IncOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let new_value = value.wrapping_add(1);

        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }
}

struct InxOperation;
impl ImpliedOperation for InxOperation {
    fn operate(&self, cpu: &Cpu) {
        let new_x = cpu.x.get().wrapping_add(1);
        cpu.set_flags(CpuFlags::Z, new_x == 0);
        cpu.set_flags(CpuFlags::N, (new_x & 0b_1000_0000) != 0);
        cpu.x.set(new_x);
    }
}

struct InyOperation;
impl ImpliedOperation for InyOperation {
    fn operate(&self, cpu: &Cpu) {
        let new_y = cpu.y.get().wrapping_add(1);
        cpu.set_flags(CpuFlags::Z, new_y == 0);
        cpu.set_flags(CpuFlags::N, (new_y & 0b_1000_0000) != 0);
        cpu.y.set(new_y);
    }
}

struct LdaOperation;
impl ReadOperation for LdaOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

struct LdxOperation;
impl ReadOperation for LdxOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.x.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

struct LdyOperation;
impl ReadOperation for LdyOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.y.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

struct LsrOperation;
impl ReadModifyWriteOperation for LsrOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let new_value = value >> 1;
        cpu.set_flags(CpuFlags::C, (value & 0b_0000_0001) != 0);
        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }
}

struct OraOperation;
impl ReadOperation for OraOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let new_a = cpu.a.get() | value;

        cpu.a.set(new_a);
        cpu.set_flags(CpuFlags::Z, new_a == 0);
        cpu.set_flags(CpuFlags::N, (new_a & 0b_1000_0000) != 0);
    }
}

struct PhaOperation;
impl StackPushOperation for PhaOperation {
    fn push(&self, cpu: &Cpu) -> u8 {
        cpu.a.get()
    }
}

struct PlaOperation;
impl StackPullOperation for PlaOperation {
    fn pull(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
    }
}

struct RolOperation;
impl ReadModifyWriteOperation for RolOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let prev_c = cpu.contains_flags(CpuFlags::C);
        let carry_mask = match prev_c {
            true  => 0b_0000_0001,
            false => 0b_0000_0000,
        };

        let new_value = (value << 1) | carry_mask;

        cpu.set_flags(CpuFlags::C, (value & 0b_1000_0000) != 0);
        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }
}

struct RorOperation;
impl ReadModifyWriteOperation for RorOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let prev_c = cpu.contains_flags(CpuFlags::C);
        let carry_mask = match prev_c {
            true  => 0b_1000_0000,
            false => 0b_0000_0000,
        };

        let new_value = (value >> 1) | carry_mask;

        cpu.a.set(new_value);
        cpu.set_flags(CpuFlags::C, (value & 0b_0000_0001) != 0);
        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }
}

struct SecOperation;
impl ImpliedOperation for SecOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::C, true);
    }
}

struct SeiOperation;
impl ImpliedOperation for SeiOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::I, true);
    }
}

struct StaOperation;
impl WriteOperation for StaOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.a.get()
    }
}

struct StxOperation;
impl WriteOperation for StxOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.x.get()
    }
}

struct StyOperation;
impl WriteOperation for StyOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.y.get()
    }
}

struct TaxOperation;
impl ImpliedOperation for TaxOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.a.get();
        cpu.x.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

struct TayOperation;
impl ImpliedOperation for TayOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.a.get();
        cpu.y.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

struct TxaOperation;
impl ImpliedOperation for TxaOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.x.get();
        cpu.a.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

struct TxsOperation;
impl ImpliedOperation for TxsOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.x.get();
        cpu.s.set(value);
    }
}

struct TyaOperation;
impl ImpliedOperation for TyaOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.y.get();
        cpu.a.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }
}

fn u16_from(lo: u8, hi: u8) -> u16 {
    ((hi as u16) << 8) | (lo as u16)
}
