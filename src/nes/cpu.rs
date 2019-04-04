use std::u8;
use std::fmt;
use std::cell::Cell;
use std::ops::Generator;
use bitflags::bitflags;
use num_derive::{FromPrimitive, ToPrimitive};
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

            let opcode = nes.read_u8(pc);
            let opcode = Opcode::from_u8(opcode);

            let op = match opcode {
                Opcode::AdcAbs => {
                    yield_all! { abs_read(nes, AdcOperation) }
                }
                Opcode::AdcAbsX => {
                    yield_all! { abs_x_read(nes, AdcOperation) }
                }
                Opcode::AdcImm => {
                    yield_all! { imm_read(nes, AdcOperation) }
                }
                Opcode::AdcZero => {
                    yield_all! { zero_read(nes, AdcOperation) }
                }
                Opcode::AndImm => {
                    yield_all! { imm_read(nes, AndOperation) }
                }
                Opcode::AndZero => {
                    yield_all! { zero_read(nes, AndOperation) }
                }
                Opcode::AndZeroX => {
                    yield_all! { zero_x_read(nes, AndOperation) }
                }
                Opcode::AslA => {
                    yield_all! { accum_modify(nes, AslOperation) }
                }
                Opcode::AslZero => {
                    yield_all! { zero_modify(nes, AslOperation) }
                }
                Opcode::Bcc => {
                    yield_all! { branch(&nes, BccOperation) }
                }
                Opcode::Bcs => {
                    yield_all! { branch(&nes, BcsOperation) }
                }
                Opcode::Beq => {
                    yield_all! { branch(&nes, BeqOperation) }
                }
                Opcode::BitZero => {
                    yield_all! { zero_read(&nes, BitOperation) }
                }
                Opcode::BitAbs => {
                    yield_all! { abs_read(&nes, BitOperation) }
                }
                Opcode::Bmi => {
                    yield_all! { branch(&nes, BmiOperation) }
                }
                Opcode::Bne => {
                    yield_all! { branch(&nes, BneOperation) }
                }
                Opcode::Bpl => {
                    yield_all! { branch(&nes, BplOperation) }
                }
                Opcode::Clc => {
                    yield_all! { implied(nes, ClcOperation) }
                }
                Opcode::Cld => {
                    yield_all! { implied(nes, CldOperation) }
                }
                Opcode::Cli => {
                    yield_all! { implied(nes, CliOperation) }
                }
                Opcode::Clv => {
                    yield_all! { implied(nes, ClvOperation) }
                }
                Opcode::CmpAbs => {
                    yield_all! { abs_x_read(nes, CmpOperation) }
                }
                Opcode::CmpAbsX => {
                    yield_all! { abs_x_read(nes, CmpOperation) }
                }
                Opcode::CmpAbsY => {
                    yield_all! { abs_y_read(nes, CmpOperation) }
                }
                Opcode::CmpImm => {
                    yield_all! { imm_read(nes, CmpOperation) }
                }
                Opcode::CmpZero => {
                    yield_all! { zero_read(nes, CmpOperation) }
                }
                Opcode::CmpZeroX => {
                    yield_all! { zero_x_read(nes, CmpOperation) }
                }
                Opcode::CpxImm => {
                    yield_all! { imm_read(nes, CpxOperation) }
                }
                Opcode::CpxZero => {
                    yield_all! { zero_read(nes, CpxOperation) }
                }
                Opcode::CpyAbs => {
                    yield_all! { abs_read(nes, CpyOperation) }
                }
                Opcode::CpyImm => {
                    yield_all! { imm_read(nes, CpyOperation) }
                }
                Opcode::DecAbs => {
                    yield_all! { abs_modify(nes, DecOperation) }
                }
                Opcode::DecAbsX => {
                    yield_all! { abs_x_modify(nes, DecOperation) }
                }
                Opcode::DecZero => {
                    yield_all! { zero_modify(nes, DecOperation) }
                }
                Opcode::DecZeroX => {
                    yield_all! { zero_x_modify(nes, DecOperation) }
                }
                Opcode::Dex => {
                    yield_all! { implied(nes, DexOperation) }
                }
                Opcode::Dey => {
                    yield_all! { implied(nes, DeyOperation) }
                }
                Opcode::EorAbs => {
                    yield_all! { abs_read(nes, EorOperation) }
                }
                Opcode::EorAbsX => {
                    yield_all! { abs_x_read(nes, EorOperation) }
                }
                Opcode::EorImm => {
                    yield_all! { imm_read(nes, EorOperation) }
                }
                Opcode::EorZero => {
                    yield_all! { zero_read(nes, EorOperation) }
                }
                Opcode::IncAbs => {
                    yield_all! { abs_modify(nes, IncOperation) }
                }
                Opcode::IncAbsX => {
                    yield_all! { abs_x_modify(nes, IncOperation) }
                }
                Opcode::IncZero => {
                    yield_all! { zero_modify(nes, IncOperation) }
                }
                Opcode::IncZeroX => {
                    yield_all! { zero_x_modify(nes, IncOperation) }
                }
                Opcode::Inx => {
                    yield_all! { implied(nes, InxOperation) }
                }
                Opcode::Iny => {
                    yield_all! { implied(nes, InyOperation) }
                }
                Opcode::JmpAbs => {
                    yield_all! { abs_jmp(nes) }
                }
                Opcode::Jsr => {
                    yield_all! { jsr(nes) }
                }
                Opcode::LdaAbs => {
                    yield_all! { abs_read(nes, LdaOperation) }
                }
                Opcode::LdaAbsX => {
                    yield_all! { abs_x_read(nes, LdaOperation) }
                }
                Opcode::LdaAbsY => {
                    yield_all! { abs_y_read(nes, LdaOperation) }
                }
                Opcode::LdaImm => {
                    yield_all! { imm_read(nes, LdaOperation) }
                }
                Opcode::LdaIndY => {
                    yield_all! { ind_y_read(nes, LdaOperation) }
                }
                Opcode::LdaZero => {
                    yield_all! { zero_read(nes, LdaOperation) }
                }
                Opcode::LdaZeroX => {
                    yield_all! { zero_x_read(nes, LdaOperation) }
                }
                Opcode::LdxAbs => {
                    yield_all! { abs_read(nes, LdxOperation) }
                }
                Opcode::LdxImm => {
                    yield_all! { imm_read(nes, LdxOperation) }
                }
                Opcode::LdxZero => {
                    yield_all! { zero_read(nes, LdxOperation) }
                }
                Opcode::LdyAbs => {
                    yield_all! { abs_read(nes, LdyOperation) }
                }
                Opcode::LdyAbsX => {
                    yield_all! { abs_x_read(nes, LdyOperation) }
                }
                Opcode::LdyImm => {
                    yield_all! { imm_read(nes, LdyOperation) }
                }
                Opcode::LdyZero => {
                    yield_all! { zero_read(nes, LdyOperation) }
                }
                Opcode::LdyZeroX => {
                    yield_all! { zero_x_read(nes, LdyOperation) }
                }
                Opcode::LsrA => {
                    yield_all! { accum_modify(nes, LsrOperation) }
                }
                Opcode::LsrZero => {
                    yield_all! { zero_modify(nes, LsrOperation) }
                }
                Opcode::Nop => {
                    yield_all! { implied(nes, NopOperation) }
                }
                Opcode::OraAbs => {
                    yield_all! { abs_read(nes, OraOperation) }
                }
                Opcode::OraImm => {
                    yield_all! { imm_read(nes, OraOperation) }
                }
                Opcode::OraZero => {
                    yield_all! { zero_read(nes, OraOperation) }
                }
                Opcode::Pha => {
                    yield_all! { stack_push(nes, PhaOperation) }
                }
                Opcode::Php => {
                    yield_all! { stack_push(nes, PhpOperation) }
                }
                Opcode::Pla => {
                    yield_all! { stack_pull(nes, PlaOperation) }
                }
                Opcode::Plp => {
                    yield_all! { stack_pull(nes, PlpOperation) }
                }
                Opcode::RolA => {
                    yield_all! { accum_modify(nes, RolOperation) }
                }
                Opcode::RorA => {
                    yield_all! { accum_modify(nes, RorOperation) }
                }
                Opcode::RorZero => {
                    yield_all! { zero_modify(nes, RorOperation) }
                }
                Opcode::Rti => {
                    yield_all! { rti(nes) }
                }
                Opcode::Rts => {
                    yield_all! { rts(nes) }
                }
                Opcode::SbcAbsX => {
                    yield_all! { abs_x_read(nes, SbcOperation) }
                }
                Opcode::SbcImm => {
                    yield_all! { imm_read(nes, SbcOperation) }
                }
                Opcode::SbcZero => {
                    yield_all! { zero_read(nes, SbcOperation) }
                }
                Opcode::Sec => {
                    yield_all! { implied(nes, SecOperation) }
                }
                Opcode::Sed => {
                    yield_all! { implied(nes, SedOperation) }
                }
                Opcode::Sei => {
                    yield_all! { implied(nes, SeiOperation) }
                }
                Opcode::StaAbs => {
                    yield_all! { abs_write(nes, StaOperation) }
                }
                Opcode::StaAbsX => {
                    yield_all! { abs_x_write(nes, StaOperation) }
                }
                Opcode::StaAbsY => {
                    yield_all! { abs_y_write(nes, StaOperation) }
                }
                Opcode::StaIndY => {
                    yield_all! { ind_y_write(nes, StaOperation) }
                }
                Opcode::StaZero => {
                    yield_all! { zero_write(nes, StaOperation) }
                }
                Opcode::StaZeroX => {
                    yield_all! { zero_x_write(nes, StaOperation) }
                }
                Opcode::StxAbs => {
                    yield_all! { abs_write(nes, StxOperation) }
                }
                Opcode::StxZero => {
                    yield_all! { zero_write(nes, StxOperation) }
                }
                Opcode::StyAbs => {
                    yield_all! { abs_write(nes, StyOperation) }
                }
                Opcode::StyZero => {
                    yield_all! { zero_write(nes, StyOperation) }
                }
                Opcode::StyZeroX => {
                    yield_all! { zero_x_write(nes, StyOperation) }
                }
                Opcode::Tax => {
                    yield_all! { implied(nes, TaxOperation) }
                }
                Opcode::Tay => {
                    yield_all! { implied(nes, TayOperation) }
                }
                Opcode::Tsx => {
                    yield_all! { implied(nes, TsxOperation) }
                }
                Opcode::Txa => {
                    yield_all! { implied(nes, TxaOperation) }
                }
                Opcode::Txs => {
                    yield_all! { implied(nes, TxsOperation) }
                }
                Opcode::Tya => {
                    yield_all! { implied(nes, TyaOperation) }
                }
                Opcode::_1A => {
                    yield_all! { implied(nes, NopOperation) }
                }
                Opcode::_3A => {
                    yield_all! { implied(nes, NopOperation) }
                }
                Opcode::_5A => {
                    yield_all! { implied(nes, NopOperation) }
                }
                Opcode::_7A => {
                    yield_all! { implied(nes, NopOperation) }
                }
                Opcode::_DA => {
                    yield_all! { implied(nes, NopOperation) }
                }
                Opcode::_FA => {
                    yield_all! { implied(nes, NopOperation) }
                }
            };

            debug_assert_eq!(op.instruction, opcode.instruction_with_mode().0);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mnemonic = match self {
            Instruction::Adc => "ADC",
            Instruction::And => "AND",
            Instruction::Asl => "ASL",
            Instruction::Bcc => "BCC",
            Instruction::Bcs => "BCS",
            Instruction::Beq => "BEQ",
            Instruction::Bit => "BIT",
            Instruction::Bmi => "BMI",
            Instruction::Bne => "BNE",
            Instruction::Bpl => "BPL",
            Instruction::Clc => "CLC",
            Instruction::Cld => "CLD",
            Instruction::Cli => "CLI",
            Instruction::Clv => "CLV",
            Instruction::Cmp => "CMP",
            Instruction::Cpx => "CPX",
            Instruction::Cpy => "CPY",
            Instruction::Dec => "DEC",
            Instruction::Dex => "DEX",
            Instruction::Dey => "DEY",
            Instruction::Eor => "EOR",
            Instruction::Inc => "INC",
            Instruction::Inx => "INX",
            Instruction::Iny => "INY",
            Instruction::Jmp => "JMP",
            Instruction::Jsr => "JSR",
            Instruction::Lda => "LDA",
            Instruction::Ldx => "LDX",
            Instruction::Ldy => "LDY",
            Instruction::Lsr => "LSR",
            Instruction::Nop => "NOP",
            Instruction::Ora => "ORA",
            Instruction::Pha => "PHA",
            Instruction::Php => "PHP",
            Instruction::Pla => "PLA",
            Instruction::Plp => "PLP",
            Instruction::Rol => "ROL",
            Instruction::Ror => "ROR",
            Instruction::Rti => "RTI",
            Instruction::Rts => "RTS",
            Instruction::Sbc => "SBC",
            Instruction::Sec => "SEC",
            Instruction::Sed => "SED",
            Instruction::Sei => "SEI",
            Instruction::Sta => "STA",
            Instruction::Stx => "STX",
            Instruction::Sty => "STY",
            Instruction::Tax => "TAX",
            Instruction::Tay => "TAY",
            Instruction::Tsx => "TSX",
            Instruction::Txa => "TXA",
            Instruction::Txs => "TXS",
            Instruction::Tya => "TYA",
        };
        write!(f, "{}", mnemonic)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpMode {
    Implied,
    Accum,
    Abs,
    AbsX,
    AbsY,
    Imm,
    IndY,
    ZeroX,
    Zero,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpArg {
    Implied,
    Accum,
    Abs { addr: u16 },
    AbsX { addr_base: u16 },
    AbsY { addr_base: u16 },
    Branch { addr_offset: i8 },
    Imm { value: u8 },
    IndY { target_addr_base: u8 },
    ZeroX { zero_page_base: u8 },
    Zero { zero_page: u8 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op {
    instruction: Instruction,
    arg: OpArg,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum Opcode {
    OraZero = 0x05,
    AslZero = 0x06,
    Php = 0x08,
    OraImm = 0x09,
    AslA = 0x0A,
    OraAbs = 0x0D,
    Bpl = 0x10,
    Clc = 0x18,
    _1A = 0x1A, // NOP (implied)
    Jsr = 0x20,
    BitZero = 0x24,
    AndZero = 0x25,
    Plp = 0x28,
    AndImm = 0x29,
    RolA = 0x2A,
    BitAbs = 0x2C,
    Bmi = 0x30,
    AndZeroX = 0x35,
    Sec = 0x38,
    _3A = 0x3A, // NOP (implied)
    Rti = 0x40,
    EorZero = 0x45,
    LsrZero = 0x46,
    Pha = 0x48,
    EorAbsX = 0x5D,
    EorImm = 0x49,
    LsrA = 0x4A,
    JmpAbs = 0x4C,
    EorAbs = 0x4D,
    Cli = 0x58,
    _5A = 0x5A,
    Rts = 0x60,
    AdcZero = 0x65,
    RorZero = 0x66,
    Pla = 0x68,
    AdcImm = 0x69,
    RorA = 0x6A,
    AdcAbs = 0x6D,
    Sei = 0x78,
    _7A = 0x7A,
    AdcAbsX = 0x7D,
    Txa = 0x8A,
    StyZero = 0x84,
    StaZero = 0x85,
    StxZero = 0x86,
    Dey = 0x88,
    StyAbs = 0x8C,
    StaAbs = 0x8D,
    StxAbs = 0x8E,
    Bcc = 0x90,
    StaIndY = 0x91,
    StyZeroX = 0x94,
    StaZeroX = 0x95,
    Tya = 0x98,
    StaAbsY = 0x99,
    Txs = 0x9A,
    StaAbsX = 0x9D,
    LdyImm = 0xA0,
    LdxImm = 0xA2,
    LdyZero = 0xA4,
    LdaZero = 0xA5,
    LdxZero = 0xA6,
    Tay = 0xA8,
    LdaImm = 0xA9,
    Tax = 0xAA,
    LdyAbs = 0xAC,
    LdaAbs = 0xAD,
    LdxAbs = 0xAE,
    Bcs = 0xB0,
    LdaIndY = 0xB1,
    LdyZeroX = 0xB4,
    LdaZeroX = 0xB5,
    Clv = 0xB8,
    LdaAbsY = 0xB9,
    Tsx = 0xBA,
    LdyAbsX = 0xBC,
    LdaAbsX = 0xBD,
    CpyImm = 0xC0,
    CmpZero = 0xC5,
    DecZero = 0xC6,
    Iny = 0xC8,
    CmpImm = 0xC9,
    Dex = 0xCA,
    CpyAbs = 0xCC,
    CmpAbs = 0xCD,
    DecAbs = 0xCE,
    Bne = 0xD0,
    CmpZeroX = 0xD5,
    DecZeroX = 0xD6,
    Cld = 0xD8,
    CmpAbsY = 0xD9,
    _DA = 0xDA,
    CmpAbsX = 0xDD,
    DecAbsX = 0xDE,
    CpxImm = 0xE0,
    CpxZero = 0xE4,
    SbcZero = 0xE5,
    IncZero = 0xE6,
    Inx = 0xE8,
    Nop = 0xEA,
    SbcImm = 0xE9,
    IncAbs = 0xEE,
    Beq = 0xF0,
    IncZeroX = 0xF6,
    Sed = 0xF8,
    _FA = 0xFA,
    SbcAbsX = 0xFD,
    IncAbsX = 0xFE,
}

impl Opcode {
    fn from_u8(opcode: u8) -> Self {
        match <Opcode as num_traits::FromPrimitive>::from_u8(opcode) {
            Some(opcode) => opcode,
            None => { unimplemented!("Unhandled opcode: 0x{:X}", opcode); }
        }
    }

    fn instruction_with_mode(&self) -> (Instruction, OpMode) {
        match self {
            Opcode::AdcAbs => (Instruction::Adc, OpMode::Abs),
            Opcode::AdcAbsX => (Instruction::Adc, OpMode::AbsX),
            Opcode::AdcImm => (Instruction::Adc, OpMode::Imm),
            Opcode::AdcZero => (Instruction::Adc, OpMode::Zero),
            Opcode::AndImm => (Instruction::And, OpMode::Imm),
            Opcode::AndZero => (Instruction::And, OpMode::Zero),
            Opcode::AndZeroX => (Instruction::And, OpMode::ZeroX),
            Opcode::AslA => (Instruction::Asl, OpMode::Accum),
            Opcode::AslZero => (Instruction::Asl, OpMode::Zero),
            Opcode::Bcc => (Instruction::Bcc, OpMode::Implied),
            Opcode::Bcs => (Instruction::Bcs, OpMode::Implied),
            Opcode::Beq => (Instruction::Beq, OpMode::Implied),
            Opcode::BitAbs => (Instruction::Bit, OpMode::Abs),
            Opcode::BitZero => (Instruction::Bit, OpMode::Zero),
            Opcode::Bmi => (Instruction::Bmi, OpMode::Implied),
            Opcode::Bne => (Instruction::Bne, OpMode::Implied),
            Opcode::Bpl => (Instruction::Bpl, OpMode::Implied),
            Opcode::Clc => (Instruction::Clc, OpMode::Implied),
            Opcode::Cld => (Instruction::Cld, OpMode::Implied),
            Opcode::Cli => (Instruction::Cli, OpMode::Implied),
            Opcode::Clv => (Instruction::Clv, OpMode::Implied),
            Opcode::CmpAbs => (Instruction::Cmp, OpMode::Abs),
            Opcode::CmpAbsX => (Instruction::Cmp, OpMode::AbsX),
            Opcode::CmpAbsY => (Instruction::Cmp, OpMode::AbsY),
            Opcode::CmpImm => (Instruction::Cmp, OpMode::Imm),
            Opcode::CmpZero => (Instruction::Cmp, OpMode::Zero),
            Opcode::CmpZeroX => (Instruction::Cmp, OpMode::ZeroX),
            Opcode::CpxImm => (Instruction::Cpx, OpMode::Imm),
            Opcode::CpxZero => (Instruction::Cpx, OpMode::Zero),
            Opcode::CpyAbs => (Instruction::Cpy, OpMode::Abs),
            Opcode::CpyImm => (Instruction::Cpy, OpMode::Imm),
            Opcode::DecAbs => (Instruction::Dec, OpMode::Abs),
            Opcode::DecAbsX => (Instruction::Dec, OpMode::AbsX),
            Opcode::DecZero => (Instruction::Dec, OpMode::Zero),
            Opcode::DecZeroX => (Instruction::Dec, OpMode::ZeroX),
            Opcode::Dex => (Instruction::Dex, OpMode::Implied),
            Opcode::Dey => (Instruction::Dey, OpMode::Implied),
            Opcode::EorAbs => (Instruction::Eor, OpMode::Abs),
            Opcode::EorAbsX => (Instruction::Eor, OpMode::AbsX),
            Opcode::EorImm => (Instruction::Eor, OpMode::Imm),
            Opcode::EorZero => (Instruction::Eor, OpMode::Zero),
            Opcode::IncAbs => (Instruction::Inc, OpMode::Abs),
            Opcode::IncAbsX => (Instruction::Inc, OpMode::AbsX),
            Opcode::IncZero => (Instruction::Inc, OpMode::Zero),
            Opcode::IncZeroX => (Instruction::Inc, OpMode::ZeroX),
            Opcode::Inx => (Instruction::Inx, OpMode::Implied),
            Opcode::Iny => (Instruction::Iny, OpMode::Implied),
            Opcode::JmpAbs => (Instruction::Jmp, OpMode::Abs),
            Opcode::Jsr => (Instruction::Jsr, OpMode::Implied),
            Opcode::LdaAbs => (Instruction::Lda, OpMode::Abs),
            Opcode::LdaAbsX => (Instruction::Lda, OpMode::AbsX),
            Opcode::LdaAbsY => (Instruction::Lda, OpMode::AbsY),
            Opcode::LdaImm => (Instruction::Lda, OpMode::Imm),
            Opcode::LdaIndY => (Instruction::Lda, OpMode::IndY),
            Opcode::LdaZero => (Instruction::Lda, OpMode::Zero),
            Opcode::LdaZeroX => (Instruction::Lda, OpMode::ZeroX),
            Opcode::LdxAbs => (Instruction::Ldx, OpMode::Abs),
            Opcode::LdxImm => (Instruction::Ldx, OpMode::Imm),
            Opcode::LdxZero => (Instruction::Ldx, OpMode::Zero),
            Opcode::LdyAbs => (Instruction::Ldy, OpMode::Abs),
            Opcode::LdyAbsX => (Instruction::Ldy, OpMode::AbsX),
            Opcode::LdyImm => (Instruction::Ldy, OpMode::Imm),
            Opcode::LdyZero => (Instruction::Ldy, OpMode::Zero),
            Opcode::LdyZeroX => (Instruction::Ldy, OpMode::ZeroX),
            Opcode::LsrA => (Instruction::Lsr, OpMode::Accum),
            Opcode::LsrZero => (Instruction::Lsr, OpMode::Zero),
            Opcode::Nop => (Instruction::Nop, OpMode::Implied),
            Opcode::OraAbs => (Instruction::Ora, OpMode::Abs),
            Opcode::OraImm => (Instruction::Ora, OpMode::Imm),
            Opcode::OraZero => (Instruction::Ora, OpMode::Zero),
            Opcode::Pha => (Instruction::Pha, OpMode::Implied),
            Opcode::Php => (Instruction::Php, OpMode::Implied),
            Opcode::Pla => (Instruction::Pla, OpMode::Implied),
            Opcode::Plp => (Instruction::Plp, OpMode::Implied),
            Opcode::RolA => (Instruction::Rol, OpMode::Accum),
            Opcode::RorA => (Instruction::Ror, OpMode::Accum),
            Opcode::RorZero => (Instruction::Ror, OpMode::Zero),
            Opcode::Rti => (Instruction::Rti, OpMode::Implied),
            Opcode::Rts => (Instruction::Rts, OpMode::Implied),
            Opcode::SbcAbsX => (Instruction::Sbc, OpMode::AbsX),
            Opcode::SbcImm => (Instruction::Sbc, OpMode::Imm),
            Opcode::SbcZero => (Instruction::Sbc, OpMode::Zero),
            Opcode::Sec => (Instruction::Sec, OpMode::Implied),
            Opcode::Sed => (Instruction::Sed, OpMode::Implied),
            Opcode::Sei => (Instruction::Sei, OpMode::Implied),
            Opcode::StaAbs => (Instruction::Sta, OpMode::Abs),
            Opcode::StaAbsX => (Instruction::Sta, OpMode::AbsX),
            Opcode::StaAbsY => (Instruction::Sta, OpMode::AbsY),
            Opcode::StaIndY => (Instruction::Sta, OpMode::IndY),
            Opcode::StaZero => (Instruction::Sta, OpMode::Zero),
            Opcode::StaZeroX => (Instruction::Sta, OpMode::ZeroX),
            Opcode::StxAbs => (Instruction::Stx, OpMode::Abs),
            Opcode::StxZero => (Instruction::Stx, OpMode::Zero),
            Opcode::StyAbs => (Instruction::Sty, OpMode::Abs),
            Opcode::StyZero => (Instruction::Sty, OpMode::Zero),
            Opcode::StyZeroX => (Instruction::Sty, OpMode::ZeroX),
            Opcode::Tax => (Instruction::Tax, OpMode::Implied),
            Opcode::Tay => (Instruction::Tay, OpMode::Implied),
            Opcode::Tsx => (Instruction::Tsx, OpMode::Implied),
            Opcode::Txa => (Instruction::Txa, OpMode::Implied),
            Opcode::Txs => (Instruction::Txs, OpMode::Implied),
            Opcode::Tya => (Instruction::Tya, OpMode::Implied),
            Opcode::_1A => (Instruction::Nop, OpMode::Implied),
            Opcode::_3A => (Instruction::Nop, OpMode::Implied),
            Opcode::_5A => (Instruction::Nop, OpMode::Implied),
            Opcode::_7A => (Instruction::Nop, OpMode::Implied),
            Opcode::_DA => (Instruction::Nop, OpMode::Implied),
            Opcode::_FA => (Instruction::Nop, OpMode::Implied),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.arg {
            OpArg::Implied => {
                write!(f, "{}", self.instruction)?;
            }
            OpArg::Accum => {
                write!(f, "{} A", self.instruction)?;
            }
            OpArg::Abs { addr } => {
                write!(f, "{} ${:04X}", self.instruction, addr)?;
            }
            OpArg::AbsX { addr_base } => {
                write!(f, "{} ${:04X},X", self.instruction, addr_base)?;
            }
            OpArg::AbsY { addr_base } => {
                write!(f, "{} ${:04X},Y", self.instruction, addr_base)?;
            }
            OpArg::Zero { zero_page } => {
                write!(f, "{} ${:02X}", self.instruction, zero_page as u16)?;
            }
            OpArg::ZeroX { zero_page_base } => {
                write!(f, "{} ${:02X},X", self.instruction, zero_page_base)?;
            }
            OpArg::IndY { target_addr_base } => {
                write!(f, "{} (${:02X}),Y", self.instruction, target_addr_base)?;
            }
            OpArg::Imm { value } => {
                write!(f, "{} #${:02X}", self.instruction, value)?;
            }
            OpArg::Branch { addr_offset } => {
                if addr_offset >= 0 {
                    write!(f, "{} _ + #${:02X}", self.instruction, addr_offset)?;
                }
                else {
                    let abs_offset = -(addr_offset as i16);
                    write!(f, "{} _ - #${:02X}", self.instruction, abs_offset)?;
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
    fn instruction(&self) -> Instruction;
}

trait ReadOperation {
    fn read(&self, cpu: &Cpu, value: u8);
    fn instruction(&self) -> Instruction;
}

trait ModifyOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8;
    fn instruction(&self) -> Instruction;
}

trait WriteOperation {
    fn write(&self, cpu: &Cpu) -> u8;
    fn instruction(&self) -> Instruction;
}

trait BranchOperation {
    fn branch(&self, cpu: &Cpu) -> bool;
    fn instruction(&self) -> Instruction;
}

trait StackPushOperation {
    fn push(&self, cpu: &Cpu) -> u8;
    fn instruction(&self) -> Instruction;
}

trait StackPullOperation {
    fn pull(&self, cpu: &Cpu, value: u8);
    fn instruction(&self) -> Instruction;
}

fn implied<'a>(
    nes: &'a Nes,
    op: impl ImpliedOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        op.operate(&nes.cpu);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::Implied,
        }
    }
}

fn accum_modify<'a>(
    nes: &'a Nes,
    op: impl ModifyOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);

        let value = nes.cpu.a.get();
        let new_value = op.modify(&nes.cpu, value);
        nes.cpu.a.set(new_value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::Accum,
        }
    }
}

fn imm_read<'a>(
    nes: &'a Nes,
    op: impl ReadOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let value = Cpu::pc_fetch_inc(nes);
        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;


        Op {
            instruction: op.instruction(),
            arg: OpArg::Imm { value },
        }
    }
}

fn zero_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Zero { zero_page },
        }
    }
}

fn zero_modify<'a>(
    nes: &'a Nes,
    op: impl ModifyOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Zero { zero_page },
        }
    }
}

fn zero_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Zero { zero_page },
        }
    }
}

fn zero_x_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::ZeroX { zero_page_base },
        }
    }
}

fn zero_x_modify<'a>(
    nes: &'a Nes,
    op: impl ModifyOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::ZeroX { zero_page_base },
        }
    }
}

fn zero_x_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::ZeroX { zero_page_base },
        }
    }
}

fn abs_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Abs { addr },
        }
    }
}

fn abs_modify<'a>(
    nes: &'a Nes,
    op: impl ModifyOperation + 'a
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Abs { addr },
        }
    }
}

fn abs_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Abs { addr },
        }
    }
}

fn abs_jmp<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: Instruction::Jmp,
            arg: OpArg::Abs { addr },
        }
    }
}

fn abs_x_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::AbsX { addr_base },
        }
    }
}

fn abs_x_modify<'a>(nes: &'a Nes, op: impl ModifyOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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
        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        let new_value = op.modify(&nes.cpu, value);
        yield CpuStep::Cycle;

        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::AbsX { addr_base },
        }
    }
}

fn abs_x_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::AbsX { addr_base },
        }
    }
}

fn abs_y_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::AbsY { addr_base },
        }
    }
}

fn abs_y_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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
        let _garbage = nes.read_u8(addr_unfixed);
        yield CpuStep::Cycle;

        let addr = addr_base.wrapping_add(y as u16);
        let new_value = op.write(&nes.cpu);
        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::AbsY { addr_base },
        }
    }
}

fn ind_y_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::IndY { target_addr_base },
        }
    }
}

fn ind_y_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::IndY { target_addr_base },
        }
    }
}

fn branch<'a>(nes: &'a Nes, op: impl BranchOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Branch { addr_offset },
        }
    }
}

fn stack_push<'a>(
    nes: &'a Nes,
    op: impl StackPushOperation + 'a,
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch(nes);
        yield CpuStep::Cycle;

        let value = op.push(&nes.cpu);
        nes.write_u8(nes.cpu.stack_addr(), value);
        nes.cpu.dec_s();

        Op {
            instruction: op.instruction(),
            arg: OpArg::Implied,
        }
    }
}

fn stack_pull<'a>(
    nes: &'a Nes,
    op: impl StackPullOperation + 'a,
)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: op.instruction(),
            arg: OpArg::Implied,
        }
    }
}

fn jsr<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: Instruction::Jsr,
            arg: OpArg::Abs { addr },
        }
    }
}

fn rti<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: Instruction::Rti,
            arg: OpArg::Implied,
        }
    }
}

fn rts<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
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

        Op {
            instruction: Instruction::Rts,
            arg: OpArg::Implied,
        }
    }
}



struct AdcOperation;
impl ReadOperation for AdcOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let AdcResult { a, c, z, v, n } = adc(AdcArg {
            a: cpu.a.get(),
            c: cpu.contains_flags(CpuFlags::C),
            value,
        });

        cpu.a.set(a);
        cpu.set_flags(CpuFlags::C, c);
        cpu.set_flags(CpuFlags::Z, z);
        cpu.set_flags(CpuFlags::V, v);
        cpu.set_flags(CpuFlags::N, n);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Adc
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

    fn instruction(&self) -> Instruction {
        Instruction::And
    }
}

struct AslOperation;
impl ModifyOperation for AslOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let c = (value & 0b_1000_0000) != 0;
        let new_value = value << 1;

        cpu.set_flags(CpuFlags::C, c);
        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::Asl
    }
}

struct BccOperation;
impl BranchOperation for BccOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::C)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bcc
    }
}

struct BcsOperation;
impl BranchOperation for BcsOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::C)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bcs
    }
}

struct BeqOperation;
impl BranchOperation for BeqOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::Z)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Beq
    }
}

struct BitOperation;
impl ReadOperation for BitOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.set_flags(CpuFlags::Z, cpu.a.get() & value == 0);
        cpu.set_flags(CpuFlags::N, value & 0b_1000_0000 != 0);
        cpu.set_flags(CpuFlags::V, value & 0b_0100_0000 != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bit
    }
}

struct BmiOperation;
impl BranchOperation for BmiOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::N)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bmi
    }
}

struct BneOperation;
impl BranchOperation for BneOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::Z)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bne
    }
}

struct BplOperation;
impl BranchOperation for BplOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::N)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bpl
    }
}

struct ClcOperation;
impl ImpliedOperation for ClcOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::C, false);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Clc
    }
}

struct CldOperation;
impl ImpliedOperation for CldOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::D, false);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Cld
    }
}

struct CliOperation;
impl ImpliedOperation for CliOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::D, false);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Cli
    }
}

struct ClvOperation;
impl ImpliedOperation for ClvOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::V, false);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Clv
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

    fn instruction(&self) -> Instruction {
        Instruction::Cmp
    }
}

struct CpxOperation;
impl ReadOperation for CpxOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let x = cpu.x.get();
        let result = x.wrapping_sub(value);

        cpu.set_flags(CpuFlags::C, x >= value);
        cpu.set_flags(CpuFlags::Z, x == value);
        cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Cpx
    }
}

struct CpyOperation;
impl ReadOperation for CpyOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        let y = cpu.y.get();
        let result = y.wrapping_sub(value);

        cpu.set_flags(CpuFlags::C, y >= value);
        cpu.set_flags(CpuFlags::Z, y == value);
        cpu.set_flags(CpuFlags::N, (result & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Cpy
    }
}

struct DecOperation;
impl ModifyOperation for DecOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let new_value = value.wrapping_sub(1);

        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::Dec
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

    fn instruction(&self) -> Instruction {
        Instruction::Dex
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

    fn instruction(&self) -> Instruction {
        Instruction::Dey
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

    fn instruction(&self) -> Instruction {
        Instruction::Eor
    }
}

struct IncOperation;
impl ModifyOperation for IncOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let new_value = value.wrapping_add(1);

        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::Inc
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

    fn instruction(&self) -> Instruction {
        Instruction::Inx
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

    fn instruction(&self) -> Instruction {
        Instruction::Iny
    }
}

struct LdaOperation;
impl ReadOperation for LdaOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Lda
    }
}

struct LdxOperation;
impl ReadOperation for LdxOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.x.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Ldx
    }
}

struct LdyOperation;
impl ReadOperation for LdyOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.y.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Ldy
    }
}

struct LsrOperation;
impl ModifyOperation for LsrOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        let new_value = value >> 1;
        cpu.set_flags(CpuFlags::C, (value & 0b_0000_0001) != 0);
        cpu.set_flags(CpuFlags::Z, new_value == 0);
        cpu.set_flags(CpuFlags::N, (new_value & 0b_1000_0000) != 0);

        new_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::Lsr
    }
}

struct NopOperation;
impl ImpliedOperation for NopOperation {
    fn operate(&self, _cpu: &Cpu) { }

    fn instruction(&self) -> Instruction {
        Instruction::Nop
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

    fn instruction(&self) -> Instruction {
        Instruction::Ora
    }
}

struct PhaOperation;
impl StackPushOperation for PhaOperation {
    fn push(&self, cpu: &Cpu) -> u8 {
        cpu.a.get()
    }

    fn instruction(&self) -> Instruction {
        Instruction::Pha
    }
}

struct PhpOperation;
impl StackPushOperation for PhpOperation {
    fn push(&self, cpu: &Cpu) -> u8 {
        cpu.p.get().bits
    }

    fn instruction(&self) -> Instruction {
        Instruction::Php
    }
}

struct PlaOperation;
impl StackPullOperation for PlaOperation {
    fn pull(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Pla
    }
}

struct PlpOperation;
impl StackPullOperation for PlpOperation {
    fn pull(&self, cpu: &Cpu, value: u8) {
        cpu.p.set(CpuFlags::from_bits_truncate(value));
    }

    fn instruction(&self) -> Instruction {
        Instruction::Plp
    }
}

struct RolOperation;
impl ModifyOperation for RolOperation {
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

    fn instruction(&self) -> Instruction {
        Instruction::Rol
    }
}

struct RorOperation;
impl ModifyOperation for RorOperation {
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

    fn instruction(&self) -> Instruction {
        Instruction::Ror
    }
}

struct SbcOperation;
impl ReadOperation for SbcOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        // Subtract-with-carry is the same as add-with-carry after
        // performing a bitwise not on `value`
        let AdcResult { a, c, z, v, n } = adc(AdcArg {
            a: cpu.a.get(),
            c: cpu.contains_flags(CpuFlags::C),
            value: !value,
        });

        cpu.a.set(a);
        cpu.set_flags(CpuFlags::C, c);
        cpu.set_flags(CpuFlags::Z, z);
        cpu.set_flags(CpuFlags::V, v);
        cpu.set_flags(CpuFlags::N, n);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Sbc
    }
}

struct SecOperation;
impl ImpliedOperation for SecOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::C, true);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Sec
    }
}

struct SedOperation;
impl ImpliedOperation for SedOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::D, true);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Sed
    }
}

struct SeiOperation;
impl ImpliedOperation for SeiOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::I, true);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Sei
    }
}

struct StaOperation;
impl WriteOperation for StaOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.a.get()
    }

    fn instruction(&self) -> Instruction {
        Instruction::Sta
    }
}

struct StxOperation;
impl WriteOperation for StxOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.x.get()
    }

    fn instruction(&self) -> Instruction {
        Instruction::Stx
    }
}

struct StyOperation;
impl WriteOperation for StyOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.y.get()
    }

    fn instruction(&self) -> Instruction {
        Instruction::Sty
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

    fn instruction(&self) -> Instruction {
        Instruction::Tax
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

    fn instruction(&self) -> Instruction {
        Instruction::Tay
    }
}

struct TsxOperation;
impl ImpliedOperation for TsxOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.s.get();
        cpu.x.set(value);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Tsx
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

    fn instruction(&self) -> Instruction {
        Instruction::Txa
    }
}

struct TxsOperation;
impl ImpliedOperation for TxsOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.x.get();
        cpu.s.set(value);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Txs
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

    fn instruction(&self) -> Instruction {
        Instruction::Tya
    }
}

struct AdcArg { a: u8, value: u8, c: bool }

struct AdcResult {
    a: u8,
    c: bool,
    z: bool,
    v: bool,
    n: bool,
}

fn adc(AdcArg { a, value, c }: AdcArg) -> AdcResult {
    let result = (a as u16).wrapping_add(value as u16).wrapping_add(c as u16);
    let out = result as u8;

    // Get the "intended" sign of the result by performing the same calculation
    // with signed integers
    let signed_a = a as i8;
    let signed_value = value as i8;
    let signed_c = c as i8;
    let signed_result = (signed_a as i16)
        .wrapping_add(signed_value as i16)
        .wrapping_add(signed_c as i16);
    let out_negative = (out & 0b_1000_0000) != 0;
    let signed_result_negative = signed_result < 0;

    let c = result >= 256;
    let z = out == 0;
    let v = out_negative != signed_result_negative;
    let n = (out & 0b_1000_0000) != 0;

    AdcResult { a: out, c, z, v, n }
}

fn u16_from(lo: u8, hi: u8) -> u16 {
    ((hi as u16) << 8) | (lo as u16)
}
