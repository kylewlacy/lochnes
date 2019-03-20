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

            let opcode = nes.read_u8(pc);
            let opcode = Opcode::from_u8(opcode);

            let op = match opcode {
                Opcode::AdcAbs => {
                    yield_all! { abs_read(nes, AdcOperation) }
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
                Opcode::CmpImm => {
                    yield_all! { imm_read(nes, CmpOperation) }
                }
                Opcode::DecAbs => {
                    yield_all! { abs_modify(nes, DecOperation) }
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
                Opcode::EorImm => {
                    yield_all! { imm_read(nes, EorOperation) }
                }
                Opcode::EorZero => {
                    yield_all! { zero_read(nes, EorOperation) }
                }
                Opcode::IncZero => {
                    yield_all! { zero_modify(nes, IncOperation) }
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
                Opcode::OraImm => {
                    yield_all! { imm_read(nes, OraOperation) }
                }
                Opcode::OraZero => {
                    yield_all! { zero_read(nes, OraOperation) }
                }
                Opcode::Pha => {
                    yield_all! { stack_push(nes, PhaOperation) }
                }
                Opcode::Pla => {
                    yield_all! { stack_pull(nes, PlaOperation) }
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
                Opcode::Sec => {
                    yield_all! { implied(nes, SecOperation) }
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
                Opcode::Tax => {
                    yield_all! { implied(nes, TaxOperation) }
                }
                Opcode::Tay => {
                    yield_all! { implied(nes, TayOperation) }
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
            };

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
    AdcAbs { addr: u16 },
    AdcImm { value: u8 },
    AdcZero { zero_page: u8 },
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
            Opcode::EorImm
            | Opcode::EorZero => "EOR",
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
            Op::AdcAbs { addr }
            | Op::DecAbs { addr }
            | Op::JmpAbs { addr }
            | Op::Jsr { addr }
            | Op::LdaAbs { addr }
            | Op::LdxAbs { addr }
            | Op::StaAbs { addr }
            | Op::StxAbs { addr }
            | Op::StyAbs { addr } => {
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
                write!(f, "{} ${:02X}", opcode, *zero_page as u16)?;
            }
            Op::AndZeroX { zero_page_base }
            | Op::DecZeroX { zero_page_base }
            | Op::LdaZeroX { zero_page_base }
            | Op::LdyZeroX { zero_page_base }
            | Op::StaZeroX { zero_page_base } => {
                write!(f, "{} ${:02X},X", opcode, zero_page_base)?;
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



enum OpArg {
    Implied,
    Imm { value: u8 },
    Zero { zero_page: u8 },
    ZeroX { zero_page_base: u8 },
    Abs { addr: u16 },
    AbsX { addr_base: u16 },
    AbsY { addr_base: u16 },
    IndY { target_addr_base: u8 },
}

struct OpBranchArg { addr_offset: i8 }

trait ImpliedOperation {
    fn operate(&self, cpu: &Cpu);
    fn operation(&self) -> Op;
}

trait ReadOperation {
    fn read(&self, cpu: &Cpu, value: u8);
    fn operation(&self, arg: OpArg) -> Op;
}

trait ModifyOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8;
    fn operation(&self, arg: OpArg) -> Op;
}

trait WriteOperation {
    fn write(&self, cpu: &Cpu) -> u8;
    fn operation(&self, arg: OpArg) -> Op;
}

trait BranchOperation {
    fn branch(&self, cpu: &Cpu) -> bool;
    fn operation(&self, arg: OpBranchArg) -> Op;
}

trait StackPushOperation {
    fn push(&self, cpu: &Cpu) -> u8;
    fn operation(&self) -> Op;
}

trait StackPullOperation {
    fn pull(&self, cpu: &Cpu, value: u8);
    fn operation(&self) -> Op;
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

        op.operation()
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

        op.operation(OpArg::Implied)
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

        op.operation(OpArg::Imm { value })
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

        op.operation(OpArg::Zero { zero_page })
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

        op.operation(OpArg::Zero { zero_page })
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

        op.operation(OpArg::Zero { zero_page })
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

        op.operation(OpArg::ZeroX { zero_page_base })
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

        op.operation(OpArg::ZeroX { zero_page_base })
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

        op.operation(OpArg::ZeroX { zero_page_base })
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

        op.operation(OpArg::Abs { addr })
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

        op.operation(OpArg::Abs { addr })
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

        op.operation(OpArg::Abs { addr })
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

        Op::JmpAbs { addr }
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

        op.operation(OpArg::AbsX { addr_base })
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

        op.operation(OpArg::AbsX { addr_base })
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

        op.operation(OpArg::AbsY { addr_base })
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

        op.operation(OpArg::IndY { target_addr_base })
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

        op.operation(OpArg::IndY { target_addr_base })
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

        op.operation(OpBranchArg { addr_offset })
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

        op.operation()
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

        op.operation()
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

        Op::Jsr { addr }
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

        Op::Rti
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

        Op::Rts
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::AdcAbs { addr },
            OpArg::Imm { value } => Op::AdcImm { value },
            OpArg::Zero { zero_page } => Op::AdcZero { zero_page },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Imm { value } => Op::AndImm { value },
            OpArg::Zero { zero_page }=> Op::AndZero { zero_page },
            OpArg::ZeroX { zero_page_base }=> Op::AndZeroX { zero_page_base },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Implied => Op::AslA,
            OpArg::Zero { zero_page } => Op::AslZero { zero_page },
            _ => { unimplemented!(); },
        }
    }
}

struct BccOperation;
impl BranchOperation for BccOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::C)
    }

    fn operation(&self, arg: OpBranchArg) -> Op {
        let OpBranchArg { addr_offset } = arg;
        Op::Bcc { addr_offset }
    }
}

struct BcsOperation;
impl BranchOperation for BcsOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::C)
    }

    fn operation(&self, arg: OpBranchArg) -> Op {
        let OpBranchArg { addr_offset } = arg;
        Op::Bcs { addr_offset }
    }
}

struct BeqOperation;
impl BranchOperation for BeqOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::Z)
    }

    fn operation(&self, arg: OpBranchArg) -> Op {
        let OpBranchArg { addr_offset } = arg;
        Op::Beq { addr_offset }
    }
}

struct BmiOperation;
impl BranchOperation for BmiOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::N)
    }

    fn operation(&self, arg: OpBranchArg) -> Op {
        let OpBranchArg { addr_offset } = arg;
        Op::Bmi { addr_offset }
    }
}

struct BneOperation;
impl BranchOperation for BneOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::Z)
    }

    fn operation(&self, arg: OpBranchArg) -> Op {
        let OpBranchArg { addr_offset } = arg;
        Op::Bne { addr_offset }
    }
}

struct BplOperation;
impl BranchOperation for BplOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::N)
    }

    fn operation(&self, arg: OpBranchArg) -> Op {
        let OpBranchArg { addr_offset } = arg;
        Op::Bpl { addr_offset }
    }
}

struct ClcOperation;
impl ImpliedOperation for ClcOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::C, false);
    }

    fn operation(&self) -> Op {
        Op::Clc
    }
}

struct CldOperation;
impl ImpliedOperation for CldOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::D, false);
    }

    fn operation(&self) -> Op {
        Op::Cld
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Imm { value } => Op::CmpImm { value },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::DecAbs { addr },
            OpArg::Zero { zero_page } => Op::DecZero { zero_page },
            OpArg::ZeroX { zero_page_base } => Op::DecZeroX { zero_page_base },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self) -> Op {
        Op::Dex
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

    fn operation(&self) -> Op {
        Op::Dey
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Imm { value } => Op::EorImm { value },
            OpArg::Zero { zero_page } => Op::EorZero { zero_page },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Zero { zero_page } => Op::IncZero { zero_page },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self) -> Op {
        Op::Inx
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

    fn operation(&self) -> Op {
        Op::Iny
    }
}

struct LdaOperation;
impl ReadOperation for LdaOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::LdaAbs { addr },
            OpArg::AbsX { addr_base } => Op::LdaAbsX { addr_base },
            OpArg::AbsY { addr_base } => Op::LdaAbsY { addr_base },
            OpArg::Imm { value } => Op::LdaImm { value },
            OpArg::Zero { zero_page } => Op::LdaZero { zero_page },
            OpArg::ZeroX { zero_page_base } => Op::LdaZeroX { zero_page_base },
            OpArg::IndY { target_addr_base } => Op::LdaIndY { target_addr_base },
            _ => { unimplemented!(); },
        }
    }
}

struct LdxOperation;
impl ReadOperation for LdxOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.x.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::LdxAbs { addr },
            OpArg::Imm { value } => Op::LdxImm { value },
            OpArg::Zero { zero_page } => Op::LdxZero { zero_page },
            _ => { unimplemented!(); },
        }
    }
}

struct LdyOperation;
impl ReadOperation for LdyOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        cpu.y.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
    }

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Imm { value } => Op::LdyImm { value },
            OpArg::Zero { zero_page } => Op::LdyZero { zero_page },
            OpArg::ZeroX { zero_page_base } => Op::LdyZeroX { zero_page_base },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Implied => Op::LsrA,
            OpArg::Zero { zero_page } => Op::LsrZero { zero_page },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Imm { value } => Op::OraImm { value },
            OpArg::Zero { zero_page } => Op::OraZero { zero_page },
            _ => { unimplemented!(); },
        }
    }
}

struct PhaOperation;
impl StackPushOperation for PhaOperation {
    fn push(&self, cpu: &Cpu) -> u8 {
        cpu.a.get()
    }

    fn operation(&self) -> Op {
        Op::Pha
    }
}

struct PlaOperation;
impl StackPullOperation for PlaOperation {
    fn pull(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
    }

    fn operation(&self) -> Op {
        Op::Pla
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Implied => Op::RolA,
            _ => { unimplemented!(); },
        }
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

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Implied => Op::RorA,
            OpArg::Zero { zero_page } => Op::RorZero { zero_page },
            _ => { unimplemented!(); },
        }
    }
}

struct SecOperation;
impl ImpliedOperation for SecOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::C, true);
    }

    fn operation(&self) -> Op {
        Op::Sec
    }
}

struct SeiOperation;
impl ImpliedOperation for SeiOperation {
    fn operate(&self, cpu: &Cpu) {
        cpu.set_flags(CpuFlags::I, true);
    }

    fn operation(&self) -> Op {
        Op::Sei
    }
}

struct StaOperation;
impl WriteOperation for StaOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.a.get()
    }

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::StaAbs { addr },
            OpArg::AbsX { addr_base } => Op::StaAbsX { addr_base },
            OpArg::Zero { zero_page } => Op::StaZero { zero_page },
            OpArg::ZeroX { zero_page_base } => Op::StaZeroX { zero_page_base },
            OpArg::IndY { target_addr_base } => Op::StaIndY { target_addr_base },
            _ => { unimplemented!(); },
        }
    }
}

struct StxOperation;
impl WriteOperation for StxOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.x.get()
    }

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::StxAbs { addr },
            OpArg::Zero { zero_page } => Op::StxZero { zero_page },
            _ => { unimplemented!(); },
        }
    }
}

struct StyOperation;
impl WriteOperation for StyOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        cpu.y.get()
    }

    fn operation(&self, arg: OpArg) -> Op {
        match arg {
            OpArg::Abs { addr } => Op::StyAbs { addr },
            OpArg::Zero { zero_page } => Op::StyZero { zero_page },
            _ => { unimplemented!(); },
        }
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

    fn operation(&self) -> Op {
        Op::Tax
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

    fn operation(&self) -> Op {
        Op::Tay
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

    fn operation(&self) -> Op {
        Op::Txa
    }
}

struct TxsOperation;
impl ImpliedOperation for TxsOperation {
    fn operate(&self, cpu: &Cpu) {
        let value = cpu.x.get();
        cpu.s.set(value);
    }

    fn operation(&self) -> Op {
        Op::Txs
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

    fn operation(&self) -> Op {
        Op::Tya
    }
}

fn u16_from(lo: u8, hi: u8) -> u16 {
    ((hi as u16) << 8) | (lo as u16)
}
