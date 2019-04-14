use std::u8;
use std::fmt;
use std::cell::Cell;
use std::ops::Generator;
use bitflags::bitflags;
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

            let instruction_with_mode = opcode_to_instruction_with_mode(opcode);
            let op = match instruction_with_mode {
                (Instruction::Adc, OpMode::Abs) => {
                    yield_all! { abs_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::Imm) => {
                    yield_all! { imm_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::Zero) => {
                    yield_all! { zero_read(nes, AdcOperation) }
                }
                (Instruction::Adc, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, AdcOperation) }
                }
                (Instruction::And, OpMode::Abs) => {
                    yield_all! { abs_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::Imm) => {
                    yield_all! { imm_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::Zero) => {
                    yield_all! { zero_read(nes, AndOperation) }
                }
                (Instruction::And, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, AndOperation) }
                }
                (Instruction::Asl, OpMode::Accum) => {
                    yield_all! { accum_modify(nes, AslOperation) }
                }
                (Instruction::Asl, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, AslOperation) }
                }
                (Instruction::Asl, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, AslOperation) }
                }
                (Instruction::Asl, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, AslOperation) }
                }
                (Instruction::Asl, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, AslOperation) }
                }
                (Instruction::Bcc, OpMode::Branch) => {
                    yield_all! { branch(&nes, BccOperation) }
                }
                (Instruction::Bcs, OpMode::Branch) => {
                    yield_all! { branch(&nes, BcsOperation) }
                }
                (Instruction::Beq, OpMode::Branch) => {
                    yield_all! { branch(&nes, BeqOperation) }
                }
                (Instruction::Bit, OpMode::Zero) => {
                    yield_all! { zero_read(&nes, BitOperation) }
                }
                (Instruction::Bit, OpMode::Abs) => {
                    yield_all! { abs_read(&nes, BitOperation) }
                }
                (Instruction::Bmi, OpMode::Branch) => {
                    yield_all! { branch(&nes, BmiOperation) }
                }
                (Instruction::Bne, OpMode::Branch) => {
                    yield_all! { branch(&nes, BneOperation) }
                }
                (Instruction::Bpl, OpMode::Branch) => {
                    yield_all! { branch(&nes, BplOperation) }
                }
                (Instruction::Brk, OpMode::Implied) => {
                    yield_all! { brk(&nes) }
                }
                (Instruction::Bvc, OpMode::Branch) => {
                    yield_all! { branch(&nes, BvcOperation) }
                }
                (Instruction::Bvs, OpMode::Branch) => {
                    yield_all! { branch(&nes, BvsOperation) }
                }
                (Instruction::Clc, OpMode::Implied) => {
                    yield_all! { implied(nes, ClcOperation) }
                }
                (Instruction::Cld, OpMode::Implied) => {
                    yield_all! { implied(nes, CldOperation) }
                }
                (Instruction::Cli, OpMode::Implied) => {
                    yield_all! { implied(nes, CliOperation) }
                }
                (Instruction::Clv, OpMode::Implied) => {
                    yield_all! { implied(nes, ClvOperation) }
                }
                (Instruction::Cmp, OpMode::Abs) => {
                    yield_all! { abs_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::Imm) => {
                    yield_all! { imm_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::Zero) => {
                    yield_all! { zero_read(nes, CmpOperation) }
                }
                (Instruction::Cmp, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, CmpOperation) }
                }
                (Instruction::Cpx, OpMode::Abs) => {
                    yield_all! { abs_read(nes, CpxOperation) }
                }
                (Instruction::Cpx, OpMode::Imm) => {
                    yield_all! { imm_read(nes, CpxOperation) }
                }
                (Instruction::Cpx, OpMode::Zero) => {
                    yield_all! { zero_read(nes, CpxOperation) }
                }
                (Instruction::Cpy, OpMode::Abs) => {
                    yield_all! { abs_read(nes, CpyOperation) }
                }
                (Instruction::Cpy, OpMode::Imm) => {
                    yield_all! { imm_read(nes, CpyOperation) }
                }
                (Instruction::Cpy, OpMode::Zero) => {
                    yield_all! { zero_read(nes, CpyOperation) }
                }
                (Instruction::Dec, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, DecOperation) }
                }
                (Instruction::Dec, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, DecOperation) }
                }
                (Instruction::Dec, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, DecOperation) }
                }
                (Instruction::Dec, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, DecOperation) }
                }
                (Instruction::Dex, OpMode::Implied) => {
                    yield_all! { implied(nes, DexOperation) }
                }
                (Instruction::Dey, OpMode::Implied) => {
                    yield_all! { implied(nes, DeyOperation) }
                }
                (Instruction::Eor, OpMode::Abs) => {
                    yield_all! { abs_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::Imm) => {
                    yield_all! { imm_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::Zero) => {
                    yield_all! { zero_read(nes, EorOperation) }
                }
                (Instruction::Eor, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, EorOperation) }
                }
                (Instruction::Inc, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, IncOperation) }
                }
                (Instruction::Inc, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, IncOperation) }
                }
                (Instruction::Inc, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, IncOperation) }
                }
                (Instruction::Inc, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, IncOperation) }
                }
                (Instruction::Inx, OpMode::Implied) => {
                    yield_all! { implied(nes, InxOperation) }
                }
                (Instruction::Iny, OpMode::Implied) => {
                    yield_all! { implied(nes, InyOperation) }
                }
                (Instruction::Jmp, OpMode::Abs) => {
                    yield_all! { abs_jmp(nes) }
                }
                (Instruction::Jmp, OpMode::Ind) => {
                    yield_all! { ind_jmp(nes) }
                }
                (Instruction::Jsr, OpMode::Abs) => {
                    yield_all! { jsr(nes) }
                }
                (Instruction::Lda, OpMode::Abs) => {
                    yield_all! { abs_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::Imm) => {
                    yield_all! { imm_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::Zero) => {
                    yield_all! { zero_read(nes, LdaOperation) }
                }
                (Instruction::Lda, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, LdaOperation) }
                }
                (Instruction::Ldx, OpMode::Abs) => {
                    yield_all! { abs_read(nes, LdxOperation) }
                }
                (Instruction::Ldx, OpMode::Imm) => {
                    yield_all! { imm_read(nes, LdxOperation) }
                }
                (Instruction::Ldx, OpMode::Zero) => {
                    yield_all! { zero_read(nes, LdxOperation) }
                }
                (Instruction::Ldx, OpMode::ZeroY) => {
                    yield_all! { zero_y_read(nes, LdxOperation) }
                }
                (Instruction::Ldy, OpMode::Abs) => {
                    yield_all! { abs_read(nes, LdyOperation) }
                }
                (Instruction::Ldy, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, LdyOperation) }
                }
                (Instruction::Ldx, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, LdxOperation) }
                }
                (Instruction::Ldy, OpMode::Imm) => {
                    yield_all! { imm_read(nes, LdyOperation) }
                }
                (Instruction::Ldy, OpMode::Zero) => {
                    yield_all! { zero_read(nes, LdyOperation) }
                }
                (Instruction::Ldy, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, LdyOperation) }
                }
                (Instruction::Lsr, OpMode::Accum) => {
                    yield_all! { accum_modify(nes, LsrOperation) }
                }
                (Instruction::Lsr, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, LsrOperation) }
                }
                (Instruction::Lsr, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, LsrOperation) }
                }
                (Instruction::Lsr, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, LsrOperation) }
                }
                (Instruction::Lsr, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, LsrOperation) }
                }
                (Instruction::Nop, OpMode::Implied) => {
                    yield_all! { implied(nes, NopOperation) }
                }
                (Instruction::Ora, OpMode::Abs) => {
                    yield_all! { abs_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::Imm) => {
                    yield_all! { imm_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::Zero) => {
                    yield_all! { zero_read(nes, OraOperation) }
                }
                (Instruction::Ora, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, OraOperation) }
                }
                (Instruction::Pha, OpMode::Implied) => {
                    yield_all! { stack_push(nes, PhaOperation) }
                }
                (Instruction::Php, OpMode::Implied) => {
                    yield_all! { stack_push(nes, PhpOperation) }
                }
                (Instruction::Pla, OpMode::Implied) => {
                    yield_all! { stack_pull(nes, PlaOperation) }
                }
                (Instruction::Plp, OpMode::Implied) => {
                    yield_all! { stack_pull(nes, PlpOperation) }
                }
                (Instruction::Rol, OpMode::Accum) => {
                    yield_all! { accum_modify(nes, RolOperation) }
                }
                (Instruction::Rol, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, RolOperation) }
                }
                (Instruction::Rol, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, RolOperation) }
                }
                (Instruction::Rol, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, RolOperation) }
                }
                (Instruction::Rol, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, RolOperation) }
                }
                (Instruction::Ror, OpMode::Accum) => {
                    yield_all! { accum_modify(nes, RorOperation) }
                }
                (Instruction::Ror, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, RorOperation) }
                }
                (Instruction::Ror, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, RorOperation) }
                }
                (Instruction::Ror, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, RorOperation) }
                }
                (Instruction::Ror, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, RorOperation) }
                }
                (Instruction::Rti, OpMode::Implied) => {
                    yield_all! { rti(nes) }
                }
                (Instruction::Rts, OpMode::Implied) => {
                    yield_all! { rts(nes) }
                }
                (Instruction::Sbc, OpMode::Abs) => {
                    yield_all! { abs_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::Imm) => {
                    yield_all! { imm_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::Zero) => {
                    yield_all! { zero_read(nes, SbcOperation) }
                }
                (Instruction::Sbc, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, SbcOperation) }
                }
                (Instruction::Sec, OpMode::Implied) => {
                    yield_all! { implied(nes, SecOperation) }
                }
                (Instruction::Sed, OpMode::Implied) => {
                    yield_all! { implied(nes, SedOperation) }
                }
                (Instruction::Sei, OpMode::Implied) => {
                    yield_all! { implied(nes, SeiOperation) }
                }
                (Instruction::Sta, OpMode::Abs) => {
                    yield_all! { abs_write(nes, StaOperation) }
                }
                (Instruction::Sta, OpMode::AbsX) => {
                    yield_all! { abs_x_write(nes, StaOperation) }
                }
                (Instruction::Sta, OpMode::AbsY) => {
                    yield_all! { abs_y_write(nes, StaOperation) }
                }
                (Instruction::Sta, OpMode::IndX) => {
                    yield_all! { ind_x_write(nes, StaOperation) }
                }
                (Instruction::Sta, OpMode::IndY) => {
                    yield_all! { ind_y_write(nes, StaOperation) }
                }
                (Instruction::Sta, OpMode::Zero) => {
                    yield_all! { zero_write(nes, StaOperation) }
                }
                (Instruction::Sta, OpMode::ZeroX) => {
                    yield_all! { zero_x_write(nes, StaOperation) }
                }
                (Instruction::Stx, OpMode::Abs) => {
                    yield_all! { abs_write(nes, StxOperation) }
                }
                (Instruction::Stx, OpMode::Zero) => {
                    yield_all! { zero_write(nes, StxOperation) }
                }
                (Instruction::Stx, OpMode::ZeroY) => {
                    yield_all! { zero_y_write(nes, StxOperation) }
                }
                (Instruction::Sty, OpMode::Abs) => {
                    yield_all! { abs_write(nes, StyOperation) }
                }
                (Instruction::Sty, OpMode::Zero) => {
                    yield_all! { zero_write(nes, StyOperation) }
                }
                (Instruction::Sty, OpMode::ZeroX) => {
                    yield_all! { zero_x_write(nes, StyOperation) }
                }
                (Instruction::Tax, OpMode::Implied) => {
                    yield_all! { implied(nes, TaxOperation) }
                }
                (Instruction::Tay, OpMode::Implied) => {
                    yield_all! { implied(nes, TayOperation) }
                }
                (Instruction::Tsx, OpMode::Implied) => {
                    yield_all! { implied(nes, TsxOperation) }
                }
                (Instruction::Txa, OpMode::Implied) => {
                    yield_all! { implied(nes, TxaOperation) }
                }
                (Instruction::Txs, OpMode::Implied) => {
                    yield_all! { implied(nes, TxsOperation) }
                }
                (Instruction::Tya, OpMode::Implied) => {
                    yield_all! { implied(nes, TyaOperation) }
                }
                (Instruction::UnofficialAnc, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialAncOperation) }
                }
                (Instruction::UnofficialAlr, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialAlrOperation) }
                }
                (Instruction::UnofficialArr, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialArrOperation) }
                }
                (Instruction::UnofficialAxs, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialAxsOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::IndX) => {
                    yield_all! { ind_x_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::IndY) => {
                    yield_all! { ind_y_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialDcp, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, UnofficialDcpOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::IndX) => {
                    yield_all! { ind_x_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::IndY) => {
                    yield_all! { ind_y_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialIsc, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, UnofficialIscOperation) }
                }
                (Instruction::UnofficialLax, OpMode::Abs) => {
                    yield_all! { abs_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialLax, OpMode::AbsY) => {
                    yield_all! { abs_y_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialLax, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialLax, OpMode::IndX) => {
                    yield_all! { ind_x_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialLax, OpMode::IndY) => {
                    yield_all! { ind_y_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialLax, OpMode::Zero) => {
                    yield_all! { zero_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialLax, OpMode::ZeroY) => {
                    yield_all! { zero_y_read(nes, UnofficialLaxOperation) }
                }
                (Instruction::UnofficialNop, OpMode::Abs) => {
                    yield_all! { abs_read(nes, UnofficialNopOperation) }
                }
                (Instruction::UnofficialNop, OpMode::AbsX) => {
                    yield_all! { abs_x_read(nes, UnofficialNopOperation) }
                }
                (Instruction::UnofficialNop, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialNopOperation) }
                }
                (Instruction::UnofficialNop, OpMode::Implied) => {
                    yield_all! { implied(nes, UnofficialNopOperation) }
                }
                (Instruction::UnofficialNop, OpMode::Zero) => {
                    yield_all! { zero_read(nes, UnofficialNopOperation) }
                }
                (Instruction::UnofficialNop, OpMode::ZeroX) => {
                    yield_all! { zero_x_read(nes, UnofficialNopOperation) }
                }
                (Instruction::UnofficialRla, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRla, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRla, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRla, OpMode::IndX) => {
                    yield_all! { ind_x_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRla, OpMode::IndY) => {
                    yield_all! { ind_y_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRla, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRla, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, UnofficialRlaOperation) }
                }
                (Instruction::UnofficialRra, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialRra, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialRra, OpMode::IndX) => {
                    yield_all! { ind_x_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialRra, OpMode::IndY) => {
                    yield_all! { ind_y_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialRra, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialRra, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialRra, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, UnofficialRraOperation) }
                }
                (Instruction::UnofficialSax, OpMode::Abs) => {
                    yield_all! { abs_write(nes, UnofficialSaxOperation) }
                }
                (Instruction::UnofficialSax, OpMode::IndX) => {
                    yield_all! { ind_x_write(nes, UnofficialSaxOperation) }
                }
                (Instruction::UnofficialSax, OpMode::Zero) => {
                    yield_all! { zero_write(nes, UnofficialSaxOperation) }
                }
                (Instruction::UnofficialSax, OpMode::ZeroY) => {
                    yield_all! { zero_y_write(nes, UnofficialSaxOperation) }
                }
                (Instruction::UnofficialSbc, OpMode::Imm) => {
                    yield_all! { imm_read(nes, UnofficialSbcOperation) }
                }
                (Instruction::UnofficialShx, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialShxOperation) }
                }
                (Instruction::UnofficialShy, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialShyOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::IndX) => {
                    yield_all! { ind_x_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::IndY) => {
                    yield_all! { ind_y_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSlo, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, UnofficialSloOperation) }
                }
                (Instruction::UnofficialSre, OpMode::Abs) => {
                    yield_all! { abs_modify(nes, UnofficialSreOperation) }
                }
                (Instruction::UnofficialSre, OpMode::AbsX) => {
                    yield_all! { abs_x_modify(nes, UnofficialSreOperation) }
                }
                (Instruction::UnofficialSre, OpMode::AbsY) => {
                    yield_all! { abs_y_modify(nes, UnofficialSreOperation) }
                }
                (Instruction::UnofficialSre, OpMode::IndX) => {
                    yield_all! { ind_x_modify(nes, UnofficialSreOperation) }
                }
                (Instruction::UnofficialSre, OpMode::IndY) => {
                    yield_all! { ind_y_modify(nes, UnofficialSreOperation) }
                }
                (Instruction::UnofficialSre, OpMode::Zero) => {
                    yield_all! { zero_modify(nes, UnofficialSreOperation) }
                }
                (Instruction::UnofficialSre, OpMode::ZeroX) => {
                    yield_all! { zero_x_modify(nes, UnofficialSreOperation) }
                }
                insn_with_mode => {
                    unimplemented!("Unhandled instruction/mode: {:?}", insn_with_mode);
                }
            };

            debug_assert_eq!(instruction_with_mode, op.instruction_with_mode());

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
    Brk,
    Bvc,
    Bvs,
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
    UnofficialAnc,
    UnofficialAlr,
    UnofficialArr,
    UnofficialAxs,
    UnofficialDcp,
    UnofficialIsc,
    UnofficialLax,
    UnofficialNop,
    UnofficialRla,
    UnofficialRra,
    UnofficialSax,
    UnofficialSbc,
    UnofficialShx,
    UnofficialShy,
    UnofficialSlo,
    UnofficialSre,
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
            Instruction::Brk => "BRK",
            Instruction::Bvc => "BVC",
            Instruction::Bvs => "BVS",
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
            Instruction::Nop | Instruction::UnofficialNop => "NOP",
            Instruction::Ora => "ORA",
            Instruction::Pha => "PHA",
            Instruction::Php => "PHP",
            Instruction::Pla => "PLA",
            Instruction::Plp => "PLP",
            Instruction::Rol => "ROL",
            Instruction::Ror => "ROR",
            Instruction::Rti => "RTI",
            Instruction::Rts => "RTS",
            Instruction::Sbc | Instruction::UnofficialSbc => "SBC",
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
            Instruction::UnofficialAnc => "ANC",
            Instruction::UnofficialAlr => "ALR",
            Instruction::UnofficialArr => "ARR",
            Instruction::UnofficialAxs => "AXS",
            Instruction::UnofficialDcp => "DCP",
            Instruction::UnofficialIsc => "ISC",
            Instruction::UnofficialLax => "LAX",
            Instruction::UnofficialRla => "RLA",
            Instruction::UnofficialRra => "RRA",
            Instruction::UnofficialSax => "SAX",
            Instruction::UnofficialShx => "SHX",
            Instruction::UnofficialShy => "SHY",
            Instruction::UnofficialSlo => "SLO",
            Instruction::UnofficialSre => "SRE",
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
    Branch,
    Imm,
    Ind,
    IndX,
    IndY,
    Zero,
    ZeroX,
    ZeroY,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpArg {
    Implied,
    Accum,
    Abs { addr: u16 },
    AbsX { addr_base: u16 },
    AbsY { addr_base: u16 },
    Branch { addr_offset: i8 },
    Ind { target_addr: u16 },
    Imm { value: u8 },
    IndX { target_addr_base: u8 },
    IndY { target_addr_base: u8 },
    Zero { zero_page: u8 },
    ZeroX { zero_page_base: u8 },
    ZeroY { zero_page_base: u8 },
}

impl From<OpArg> for OpMode {
    fn from(arg: OpArg) -> Self {
        match arg {
            OpArg::Implied => OpMode::Implied,
            OpArg::Accum => OpMode::Accum,
            OpArg::Abs { .. } => OpMode::Abs,
            OpArg::AbsX { .. } => OpMode::AbsX,
            OpArg::AbsY { .. } => OpMode::AbsY,
            OpArg::Branch { .. } => OpMode::Branch,
            OpArg::Imm { .. } => OpMode::Imm,
            OpArg::Ind { .. } => OpMode::Ind,
            OpArg::IndX { .. } => OpMode::IndX,
            OpArg::IndY { .. } => OpMode::IndY,
            OpArg::Zero { .. } => OpMode::Zero,
            OpArg::ZeroX { .. } => OpMode::ZeroX,
            OpArg::ZeroY { .. } => OpMode::ZeroY,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op {
    instruction: Instruction,
    arg: OpArg,
}

impl Op {
    fn instruction_with_mode(&self) -> (Instruction, OpMode) {
        (self.instruction, self.arg.into())
    }
}

fn opcode_to_instruction_with_mode(opcode: u8) -> (Instruction, OpMode) {
    match opcode {
        0x00 => (Instruction::Brk, OpMode::Implied),
        0x01 => (Instruction::Ora, OpMode::IndX),
        0x03 => (Instruction::UnofficialSlo, OpMode::IndX),
        0x04 => (Instruction::UnofficialNop, OpMode::Zero),
        0x05 => (Instruction::Ora, OpMode::Zero),
        0x06 => (Instruction::Asl, OpMode::Zero),
        0x07 => (Instruction::UnofficialSlo, OpMode::Zero),
        0x08 => (Instruction::Php, OpMode::Implied),
        0x09 => (Instruction::Ora, OpMode::Imm),
        0x0A => (Instruction::Asl, OpMode::Accum),
        0x0B => (Instruction::UnofficialAnc, OpMode::Imm),
        0x0C => (Instruction::UnofficialNop, OpMode::Abs),
        0x0D => (Instruction::Ora, OpMode::Abs),
        0x0E => (Instruction::Asl, OpMode::Abs),
        0x0F => (Instruction::UnofficialSlo, OpMode::Abs),
        0x10 => (Instruction::Bpl, OpMode::Branch),
        0x11 => (Instruction::Ora, OpMode::IndY),
        0x13 => (Instruction::UnofficialSlo, OpMode::IndY),
        0x14 => (Instruction::UnofficialNop, OpMode::ZeroX),
        0x15 => (Instruction::Ora, OpMode::ZeroX),
        0x16 => (Instruction::Asl, OpMode::ZeroX),
        0x17 => (Instruction::UnofficialSlo, OpMode::ZeroX),
        0x18 => (Instruction::Clc, OpMode::Implied),
        0x19 => (Instruction::Ora, OpMode::AbsY),
        0x1A => (Instruction::UnofficialNop, OpMode::Implied),
        0x1B => (Instruction::UnofficialSlo, OpMode::AbsY),
        0x1C => (Instruction::UnofficialNop, OpMode::AbsX),
        0x1D => (Instruction::Ora, OpMode::AbsX),
        0x1E => (Instruction::Asl, OpMode::AbsX),
        0x1F => (Instruction::UnofficialSlo, OpMode::AbsX),
        0x20 => (Instruction::Jsr, OpMode::Abs),
        0x21 => (Instruction::And, OpMode::IndX),
        0x23 => (Instruction::UnofficialRla, OpMode::IndX),
        0x24 => (Instruction::Bit, OpMode::Zero),
        0x25 => (Instruction::And, OpMode::Zero),
        0x26 => (Instruction::Rol, OpMode::Zero),
        0x27 => (Instruction::UnofficialRla, OpMode::Zero),
        0x28 => (Instruction::Plp, OpMode::Implied),
        0x29 => (Instruction::And, OpMode::Imm),
        0x2A => (Instruction::Rol, OpMode::Accum),
        0x2B => (Instruction::UnofficialAnc, OpMode::Imm),
        0x2C => (Instruction::Bit, OpMode::Abs),
        0x2D => (Instruction::And, OpMode::Abs),
        0x2E => (Instruction::Rol, OpMode::Abs),
        0x2F => (Instruction::UnofficialRla, OpMode::Abs),
        0x30 => (Instruction::Bmi, OpMode::Branch),
        0x31 => (Instruction::And, OpMode::IndY),
        0x33 => (Instruction::UnofficialRla, OpMode::IndY),
        0x34 => (Instruction::UnofficialNop, OpMode::ZeroX),
        0x35 => (Instruction::And, OpMode::ZeroX),
        0x36 => (Instruction::Rol, OpMode::ZeroX),
        0x37 => (Instruction::UnofficialRla, OpMode::ZeroX),
        0x38 => (Instruction::Sec, OpMode::Implied),
        0x39 => (Instruction::And, OpMode::AbsY),
        0x3A => (Instruction::UnofficialNop, OpMode::Implied),
        0x3B => (Instruction::UnofficialRla, OpMode::AbsY),
        0x3C => (Instruction::UnofficialNop, OpMode::AbsX),
        0x3D => (Instruction::And, OpMode::AbsX),
        0x3E => (Instruction::Rol, OpMode::AbsX),
        0x3F => (Instruction::UnofficialRla, OpMode::AbsX),
        0x40 => (Instruction::Rti, OpMode::Implied),
        0x41 => (Instruction::Eor, OpMode::IndX),
        0x43 => (Instruction::UnofficialSre, OpMode::IndX),
        0x44 => (Instruction::UnofficialNop, OpMode::Zero),
        0x45 => (Instruction::Eor, OpMode::Zero),
        0x46 => (Instruction::Lsr, OpMode::Zero),
        0x47 => (Instruction::UnofficialSre, OpMode::Zero),
        0x48 => (Instruction::Pha, OpMode::Implied),
        0x49 => (Instruction::Eor, OpMode::Imm),
        0x4A => (Instruction::Lsr, OpMode::Accum),
        0x4B => (Instruction::UnofficialAlr, OpMode::Imm),
        0x4C => (Instruction::Jmp, OpMode::Abs),
        0x4D => (Instruction::Eor, OpMode::Abs),
        0x4E => (Instruction::Lsr, OpMode::Abs),
        0x4F => (Instruction::UnofficialSre, OpMode::Abs),
        0x50 => (Instruction::Bvc, OpMode::Branch),
        0x51 => (Instruction::Eor, OpMode::IndY),
        0x53 => (Instruction::UnofficialSre, OpMode::IndY),
        0x54 => (Instruction::UnofficialNop, OpMode::ZeroX),
        0x55 => (Instruction::Eor, OpMode::ZeroX),
        0x56 => (Instruction::Lsr, OpMode::ZeroX),
        0x57 => (Instruction::UnofficialSre, OpMode::ZeroX),
        0x58 => (Instruction::Cli, OpMode::Implied),
        0x59 => (Instruction::Eor, OpMode::AbsY),
        0x5A => (Instruction::UnofficialNop, OpMode::Implied),
        0x5B => (Instruction::UnofficialSre, OpMode::AbsY),
        0x5C => (Instruction::UnofficialNop, OpMode::AbsX),
        0x5D => (Instruction::Eor, OpMode::AbsX),
        0x5E => (Instruction::Lsr, OpMode::AbsX),
        0x5F => (Instruction::UnofficialSre, OpMode::AbsX),
        0x60 => (Instruction::Rts, OpMode::Implied),
        0x61 => (Instruction::Adc, OpMode::IndX),
        0x64 => (Instruction::UnofficialNop, OpMode::Zero),
        0x63 => (Instruction::UnofficialRra, OpMode::IndX),
        0x65 => (Instruction::Adc, OpMode::Zero),
        0x66 => (Instruction::Ror, OpMode::Zero),
        0x67 => (Instruction::UnofficialRra, OpMode::Zero),
        0x68 => (Instruction::Pla, OpMode::Implied),
        0x69 => (Instruction::Adc, OpMode::Imm),
        0x6A => (Instruction::Ror, OpMode::Accum),
        0x6B => (Instruction::UnofficialArr, OpMode::Imm),
        0x6C => (Instruction::Jmp, OpMode::Ind),
        0x6D => (Instruction::Adc, OpMode::Abs),
        0x6E => (Instruction::Ror, OpMode::Abs),
        0x6F => (Instruction::UnofficialRra, OpMode::Abs),
        0x70 => (Instruction::Bvs, OpMode::Branch),
        0x71 => (Instruction::Adc, OpMode::IndY),
        0x73 => (Instruction::UnofficialRra, OpMode::IndY),
        0x74 => (Instruction::UnofficialNop, OpMode::ZeroX),
        0x75 => (Instruction::Adc, OpMode::ZeroX),
        0x76 => (Instruction::Ror, OpMode::ZeroX),
        0x77 => (Instruction::UnofficialRra, OpMode::ZeroX),
        0x78 => (Instruction::Sei, OpMode::Implied),
        0x79 => (Instruction::Adc, OpMode::AbsY),
        0x7A => (Instruction::UnofficialNop, OpMode::Implied),
        0x7B => (Instruction::UnofficialRra, OpMode::AbsY),
        0x7C => (Instruction::UnofficialNop, OpMode::AbsX),
        0x7D => (Instruction::Adc, OpMode::AbsX),
        0x7E => (Instruction::Ror, OpMode::AbsX),
        0x7F => (Instruction::UnofficialRra, OpMode::AbsX),
        0x80 => (Instruction::UnofficialNop, OpMode::Imm),
        0x81 => (Instruction::Sta, OpMode::IndX),
        0x82 => (Instruction::UnofficialNop, OpMode::Imm),
        0x83 => (Instruction::UnofficialSax, OpMode::IndX),
        0x84 => (Instruction::Sty, OpMode::Zero),
        0x85 => (Instruction::Sta, OpMode::Zero),
        0x86 => (Instruction::Stx, OpMode::Zero),
        0x87 => (Instruction::UnofficialSax, OpMode::Zero),
        0x88 => (Instruction::Dey, OpMode::Implied),
        0x89 => (Instruction::UnofficialNop, OpMode::Imm),
        0x8A => (Instruction::Txa, OpMode::Implied),
        0x8C => (Instruction::Sty, OpMode::Abs),
        0x8D => (Instruction::Sta, OpMode::Abs),
        0x8E => (Instruction::Stx, OpMode::Abs),
        0x8F => (Instruction::UnofficialSax, OpMode::Abs),
        0x90 => (Instruction::Bcc, OpMode::Branch),
        0x91 => (Instruction::Sta, OpMode::IndY),
        0x94 => (Instruction::Sty, OpMode::ZeroX),
        0x95 => (Instruction::Sta, OpMode::ZeroX),
        0x96 => (Instruction::Stx, OpMode::ZeroY),
        0x97 => (Instruction::UnofficialSax, OpMode::ZeroY),
        0x98 => (Instruction::Tya, OpMode::Implied),
        0x99 => (Instruction::Sta, OpMode::AbsY),
        0x9A => (Instruction::Txs, OpMode::Implied),
        0x9C => (Instruction::UnofficialShy, OpMode::AbsX),
        0x9D => (Instruction::Sta, OpMode::AbsX),
        0x9E => (Instruction::UnofficialShx, OpMode::AbsY),
        0xA0 => (Instruction::Ldy, OpMode::Imm),
        0xA1 => (Instruction::Lda, OpMode::IndX),
        0xA2 => (Instruction::Ldx, OpMode::Imm),
        0xA3 => (Instruction::UnofficialLax, OpMode::IndX),
        0xA4 => (Instruction::Ldy, OpMode::Zero),
        0xA5 => (Instruction::Lda, OpMode::Zero),
        0xA6 => (Instruction::Ldx, OpMode::Zero),
        0xA7 => (Instruction::UnofficialLax, OpMode::Zero),
        0xA8 => (Instruction::Tay, OpMode::Implied),
        0xA9 => (Instruction::Lda, OpMode::Imm),
        0xAA => (Instruction::Tax, OpMode::Implied),
        0xAB => (Instruction::UnofficialLax, OpMode::Imm),
        0xAC => (Instruction::Ldy, OpMode::Abs),
        0xAD => (Instruction::Lda, OpMode::Abs),
        0xAE => (Instruction::Ldx, OpMode::Abs),
        0xAF => (Instruction::UnofficialLax, OpMode::Abs),
        0xB0 => (Instruction::Bcs, OpMode::Branch),
        0xB1 => (Instruction::Lda, OpMode::IndY),
        0xB3 => (Instruction::UnofficialLax, OpMode::IndY),
        0xB4 => (Instruction::Ldy, OpMode::ZeroX),
        0xB5 => (Instruction::Lda, OpMode::ZeroX),
        0xB6 => (Instruction::Ldx, OpMode::ZeroY),
        0xB7 => (Instruction::UnofficialLax, OpMode::ZeroY),
        0xB8 => (Instruction::Clv, OpMode::Implied),
        0xB9 => (Instruction::Lda, OpMode::AbsY),
        0xBA => (Instruction::Tsx, OpMode::Implied),
        0xBC => (Instruction::Ldy, OpMode::AbsX),
        0xBD => (Instruction::Lda, OpMode::AbsX),
        0xBE => (Instruction::Ldx, OpMode::AbsY),
        0xBF => (Instruction::UnofficialLax, OpMode::AbsY),
        0xC0 => (Instruction::Cpy, OpMode::Imm),
        0xC1 => (Instruction::Cmp, OpMode::IndX),
        0xC2 => (Instruction::UnofficialNop, OpMode::Imm),
        0xC3 => (Instruction::UnofficialDcp, OpMode::IndX),
        0xC4 => (Instruction::Cpy, OpMode::Zero),
        0xC5 => (Instruction::Cmp, OpMode::Zero),
        0xC6 => (Instruction::Dec, OpMode::Zero),
        0xC7 => (Instruction::UnofficialDcp, OpMode::Zero),
        0xC8 => (Instruction::Iny, OpMode::Implied),
        0xC9 => (Instruction::Cmp, OpMode::Imm),
        0xCA => (Instruction::Dex, OpMode::Implied),
        0xCB => (Instruction::UnofficialAxs, OpMode::Imm),
        0xCC => (Instruction::Cpy, OpMode::Abs),
        0xCD => (Instruction::Cmp, OpMode::Abs),
        0xCE => (Instruction::Dec, OpMode::Abs),
        0xCF => (Instruction::UnofficialDcp, OpMode::Abs),
        0xD0 => (Instruction::Bne, OpMode::Branch),
        0xD1 => (Instruction::Cmp, OpMode::IndY),
        0xD3 => (Instruction::UnofficialDcp, OpMode::IndY),
        0xD4 => (Instruction::UnofficialNop, OpMode::ZeroX),
        0xD5 => (Instruction::Cmp, OpMode::ZeroX),
        0xD6 => (Instruction::Dec, OpMode::ZeroX),
        0xD7 => (Instruction::UnofficialDcp, OpMode::ZeroX),
        0xD8 => (Instruction::Cld, OpMode::Implied),
        0xD9 => (Instruction::Cmp, OpMode::AbsY),
        0xDA => (Instruction::UnofficialNop, OpMode::Implied),
        0xDB => (Instruction::UnofficialDcp, OpMode::AbsY),
        0xDC => (Instruction::UnofficialNop, OpMode::AbsX),
        0xDD => (Instruction::Cmp, OpMode::AbsX),
        0xDE => (Instruction::Dec, OpMode::AbsX),
        0xDF => (Instruction::UnofficialDcp, OpMode::AbsX),
        0xE0 => (Instruction::Cpx, OpMode::Imm),
        0xE1 => (Instruction::Sbc, OpMode::IndX),
        0xE2 => (Instruction::UnofficialNop, OpMode::Imm),
        0xE3 => (Instruction::UnofficialIsc, OpMode::IndX),
        0xE4 => (Instruction::Cpx, OpMode::Zero),
        0xE5 => (Instruction::Sbc, OpMode::Zero),
        0xE6 => (Instruction::Inc, OpMode::Zero),
        0xE7 => (Instruction::UnofficialIsc, OpMode::Zero),
        0xE8 => (Instruction::Inx, OpMode::Implied),
        0xE9 => (Instruction::Sbc, OpMode::Imm),
        0xEA => (Instruction::Nop, OpMode::Implied),
        0xEB => (Instruction::UnofficialSbc, OpMode::Imm),
        0xEC => (Instruction::Cpx, OpMode::Abs),
        0xED => (Instruction::Sbc, OpMode::Abs),
        0xEE => (Instruction::Inc, OpMode::Abs),
        0xEF => (Instruction::UnofficialIsc, OpMode::Abs),
        0xF0 => (Instruction::Beq, OpMode::Branch),
        0xF1 => (Instruction::Sbc, OpMode::IndY),
        0xF3 => (Instruction::UnofficialIsc, OpMode::IndY),
        0xF4 => (Instruction::UnofficialNop, OpMode::ZeroX),
        0xF5 => (Instruction::Sbc, OpMode::ZeroX),
        0xF6 => (Instruction::Inc, OpMode::ZeroX),
        0xF7 => (Instruction::UnofficialIsc, OpMode::ZeroX),
        0xF8 => (Instruction::Sed, OpMode::Implied),
        0xF9 => (Instruction::Sbc, OpMode::AbsY),
        0xFA => (Instruction::UnofficialNop, OpMode::Implied),
        0xFB => (Instruction::UnofficialIsc, OpMode::AbsY),
        0xFC => (Instruction::UnofficialNop, OpMode::AbsX),
        0xFD => (Instruction::Sbc, OpMode::AbsX),
        0xFE => (Instruction::Inc, OpMode::AbsX),
        0xFF => (Instruction::UnofficialIsc, OpMode::AbsX),
        _ => { unimplemented!("Unhandled opcode: 0x{:X}", opcode); },
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
            OpArg::ZeroY { zero_page_base } => {
                write!(f, "{} ${:02X},Y", self.instruction, zero_page_base)?;
            }
            OpArg::Ind { target_addr } => {
                write!(f, "{} (${:04X})", self.instruction, target_addr)?;
            }
            OpArg::IndX { target_addr_base } => {
                write!(f, "{} (${:02X},X)", self.instruction, target_addr_base)?;
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

fn brk<'a>(nes: &'a Nes) -> impl Generator<Yield = CpuStep, Return = Op> + 'a {
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let pc = nes.cpu.pc.get();
        nes.push_u8(((pc & 0xFF00) >> 8) as u8);
        yield CpuStep::Cycle;

        nes.push_u8((pc & 0x00FF) as u8);
        yield CpuStep::Cycle;

        let p = nes.cpu.p.get().bits | 0b_0011_0000;
        nes.push_u8(p);
        yield CpuStep::Cycle;

        let pc_hi = nes.read_u8(0xFFFE);
        yield CpuStep::Cycle;

        let pc_lo = nes.read_u8(0xFFFF);
        let pc = u16_from(pc_hi, pc_lo);
        nes.cpu.pc.set(pc);
        yield CpuStep::Cycle;

        Op {
            instruction: Instruction::Brk,
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

fn zero_y_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(zero_page_base as u16);
        let addr = zero_page_base.wrapping_add(nes.cpu.y.get()) as u16;
        yield CpuStep::Cycle;

        let value = nes.read_u8(addr);
        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::ZeroY { zero_page_base },
        }
    }
}

fn zero_y_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let zero_page_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(zero_page_base as u16);
        let addr = zero_page_base.wrapping_add(nes.cpu.y.get()) as u16;
        yield CpuStep::Cycle;

        let value = op.write(&nes.cpu);
        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::ZeroY { zero_page_base },
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

fn ind_jmp<'a>(nes: &'a Nes)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let target_lo = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let target_hi = Cpu::pc_fetch(nes);
        let addr_lo = u16_from(target_lo, target_hi);
        let addr_hi = u16_from(target_lo.wrapping_add(1), target_hi);
        yield CpuStep::Cycle;

        let pc_lo = nes.read_u8(addr_lo);
        yield CpuStep::Cycle;

        let pc_hi = nes.read_u8(addr_hi);
        let addr = u16_from(pc_lo, pc_hi);
        nes.cpu.pc.set(addr);
        yield CpuStep::Cycle;

        Op {
            instruction: Instruction::Jmp,
            arg: OpArg::Ind { target_addr: addr_lo },
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

        // Speculatively load from memory based on the
        // incomplete address calculation
        let addr_unfixed = u16_from(addr_lo_x, addr_hi);
        let value_unfixed = nes.read_u8(addr_unfixed);

        // Calculate the actual address to use
        let addr = addr_base.wrapping_add(x as u16);
        let value =
            if addr == addr_unfixed {
                value_unfixed
            }
            else {
                // If the speculative load was incorrect, read the from
                // the correct address (at the cost of an extra cycle)
                let fixed_value = nes.read_u8(addr);
                yield CpuStep::Cycle;

                fixed_value
            };

        op.read(&nes.cpu, value);
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

        // Speculatively load from memory based on the
        // incomplete address calculation
        let addr_unfixed = u16_from(addr_lo_y, addr_hi);
        let value_unfixed = nes.read_u8(addr_unfixed);

        // Calculate the actual address to use
        let addr = addr_base.wrapping_add(y as u16);
        let value =
            if addr == addr_unfixed {
                value_unfixed
            }
            else {
                // If the speculative load was incorrect, read the from
                // the correct address (at the cost of an extra cycle)
                let fixed_value = nes.read_u8(addr);
                yield CpuStep::Cycle;

                fixed_value
            };

        op.read(&nes.cpu, value);
        Op {
            instruction: op.instruction(),
            arg: OpArg::AbsY { addr_base },
        }
    }
}

fn abs_y_modify<'a>(nes: &'a Nes, op: impl ModifyOperation + 'a)
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
        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        let new_value = op.modify(&nes.cpu, value);
        yield CpuStep::Cycle;

        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

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

fn ind_x_read<'a>(nes: &'a Nes, op: impl ReadOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let target_addr_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(target_addr_base as u16);
        let x = nes.cpu.x.get();
        let target_addr = target_addr_base.wrapping_add(x);
        yield CpuStep::Cycle;

        let addr_lo = nes.read_u8(target_addr as u16);
        yield CpuStep::Cycle;

        let addr_hi = nes.read_u8(target_addr.wrapping_add(1) as u16);
        yield CpuStep::Cycle;

        let addr = u16_from(addr_lo, addr_hi);
        let value = nes.read_u8(addr);
        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::IndX { target_addr_base },
        }
    }
}

fn ind_x_modify<'a>(nes: &'a Nes, op: impl ModifyOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let target_addr_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(target_addr_base as u16);
        let x = nes.cpu.x.get();
        let target_addr = target_addr_base.wrapping_add(x);
        yield CpuStep::Cycle;

        let addr_lo = nes.read_u8(target_addr as u16);
        yield CpuStep::Cycle;

        let addr_hi = nes.read_u8(target_addr.wrapping_add(1) as u16);
        yield CpuStep::Cycle;

        let addr = u16_from(addr_lo, addr_hi);
        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        let new_value = op.modify(&nes.cpu, value);
        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::IndX { target_addr_base },
        }
    }
}

fn ind_x_write<'a>(nes: &'a Nes, op: impl WriteOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let target_addr_base = Cpu::pc_fetch_inc(nes);
        yield CpuStep::Cycle;

        let _garbage = nes.read_u8(target_addr_base as u16);
        let x = nes.cpu.x.get();
        let target_addr = target_addr_base.wrapping_add(x);
        yield CpuStep::Cycle;

        let addr_lo = nes.read_u8(target_addr as u16);
        yield CpuStep::Cycle;

        let addr_hi = nes.read_u8(target_addr.wrapping_add(1) as u16);
        yield CpuStep::Cycle;

        let addr = u16_from(addr_lo, addr_hi);
        let value = op.write(&nes.cpu);
        nes.write_u8(addr, value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::IndX { target_addr_base },
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
        let target_addr_base_hi = target_addr_base.wrapping_add(1) as u16;
        yield CpuStep::Cycle;

        let addr_base_lo = nes.read_u8(target_addr_base_lo);
        yield CpuStep::Cycle;

        let addr_base_hi = nes.read_u8(target_addr_base_hi);
        let y = nes.cpu.y.get();

        yield CpuStep::Cycle;

        // Speculatively load from memory based on the
        // incomplete address calculation
        let addr_unfixed = u16_from(addr_base_lo.wrapping_add(y), addr_base_hi);
        let value_unfixed = nes.read_u8(addr_unfixed);
        yield CpuStep::Cycle;

        // Calculate the actual address to use
        let addr_base = u16_from(addr_base_lo, addr_base_hi);
        let addr = addr_base.wrapping_add(y as u16);
        let value =
            if addr == addr_unfixed {
                value_unfixed
            }
            else {
                // If the speculative load was incorrect, read the from
                // the correct address (at the cost of an extra cycle)
                let fixed_value = nes.read_u8(addr);
                yield CpuStep::Cycle;

                fixed_value
            };

        op.read(&nes.cpu, value);
        yield CpuStep::Cycle;

        Op {
            instruction: op.instruction(),
            arg: OpArg::IndY { target_addr_base },
        }
    }
}

fn ind_y_modify<'a>(nes: &'a Nes, op: impl ModifyOperation + 'a)
    -> impl Generator<Yield = CpuStep, Return = Op> + 'a
{
    move || {
        let _opcode = Cpu::pc_fetch_inc(&nes);
        yield CpuStep::Cycle;

        let target_addr_base = Cpu::pc_fetch_inc(&nes);
        let target_addr_base_lo = target_addr_base as u16;
        let target_addr_base_hi = target_addr_base.wrapping_add(1) as u16;
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
        let value = nes.read_u8(addr);
        yield CpuStep::Cycle;

        nes.write_u8(addr, value);
        let new_value = op.modify(&nes.cpu, value);
        yield CpuStep::Cycle;

        nes.write_u8(addr, new_value);
        yield CpuStep::Cycle;

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
        let target_addr_base_hi = target_addr_base.wrapping_add(1) as u16;
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

struct BvcOperation;
impl BranchOperation for BvcOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        !cpu.contains_flags(CpuFlags::V)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bvc
    }
}

struct BvsOperation;
impl BranchOperation for BvsOperation {
    fn branch(&self, cpu: &Cpu) -> bool {
        cpu.contains_flags(CpuFlags::V)
    }

    fn instruction(&self) -> Instruction {
        Instruction::Bvs
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
        cpu.set_flags(CpuFlags::I, false);
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

        cpu.x.set(new_x);
        cpu.set_flags(CpuFlags::Z, new_x == 0);
        cpu.set_flags(CpuFlags::N, (new_x & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Dex
    }
}

struct DeyOperation;
impl ImpliedOperation for DeyOperation {
    fn operate(&self, cpu: &Cpu) {
        let new_y = cpu.y.get().wrapping_sub(1);

        cpu.y.set(new_y);
        cpu.set_flags(CpuFlags::Z, new_y == 0);
        cpu.set_flags(CpuFlags::N, (new_y & 0b_1000_0000) != 0);
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

        cpu.x.set(new_x);
        cpu.set_flags(CpuFlags::Z, new_x == 0);
        cpu.set_flags(CpuFlags::N, (new_x & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::Inx
    }
}

struct InyOperation;
impl ImpliedOperation for InyOperation {
    fn operate(&self, cpu: &Cpu) {
        let new_y = cpu.y.get().wrapping_add(1);

        cpu.y.set(new_y);
        cpu.set_flags(CpuFlags::Z, new_y == 0);
        cpu.set_flags(CpuFlags::N, (new_y & 0b_1000_0000) != 0);
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
        cpu.p.get().bits | 0b_0011_0000
    }

    fn instruction(&self) -> Instruction {
        Instruction::Php
    }
}

struct PlaOperation;
impl StackPullOperation for PlaOperation {
    fn pull(&self, cpu: &Cpu, value: u8) {
        cpu.a.set(value);
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
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
        cpu.set_flags(CpuFlags::Z, value == 0);
        cpu.set_flags(CpuFlags::N, (value & 0b_1000_0000) != 0);
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

struct UnofficialAncOperation;
impl ReadOperation for UnofficialAncOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        // This operation is like AND, but the C flag is also set to
        // bit 7 of the result
        let new_a = cpu.a.get() & value;

        cpu.a.set(new_a);
        cpu.set_flags(CpuFlags::Z, new_a == 0);
        cpu.set_flags(CpuFlags::N, (new_a & 0b_1000_0000) != 0);
        cpu.set_flags(CpuFlags::C, (new_a & 0b_1000_0000) != 0);
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialAnc
    }
}

struct UnofficialAlrOperation;
impl ReadOperation for UnofficialAlrOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        // This operation combines AND and LSR
        AndOperation.read(cpu, value);
        cpu.a.set(LsrOperation.modify(cpu, cpu.a.get()));
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialAlr
    }
}

struct UnofficialArrOperation;
impl ReadOperation for UnofficialArrOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        // This operation is a combination of AND and ROR, except it
        // sets the C and V flags differently
        let prev_c = cpu.contains_flags(CpuFlags::C);
        let carry_mask = match prev_c {
            true  => 0b_1000_0000,
            false => 0b_0000_0000,
        };

        let and_a = cpu.a.get() & value;
        let result = (and_a >> 1) | carry_mask;
        let result_bit_7 = (result & 0b_1000_0000) != 0;
        let result_bit_6 = (result & 0b_0100_0000) != 0;
        let result_bit_5 = (result & 0b_0010_0000) != 0;

        cpu.a.set(result);
        cpu.set_flags(CpuFlags::C, result_bit_6);
        cpu.set_flags(CpuFlags::Z, result == 0);
        cpu.set_flags(CpuFlags::V, result_bit_6 != result_bit_5);
        cpu.set_flags(CpuFlags::N, result_bit_7);
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialArr
    }
}

struct UnofficialAxsOperation;
impl ReadOperation for UnofficialAxsOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        // This operation sets X to ((A & X) - value), and updates the
        // C, Z, and N flags appropriately.
        let a_and_x = cpu.a.get() & cpu.x.get();
        let AdcResult { a: result, c, z, v: _, n } = adc(AdcArg {
            a: a_and_x,
            c: true,
            value: !value
        });

        cpu.x.set(result);
        cpu.set_flags(CpuFlags::C, c);
        cpu.set_flags(CpuFlags::Z, z);
        cpu.set_flags(CpuFlags::N, n);
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialAxs
    }
}

struct UnofficialDcpOperation;
impl ModifyOperation for UnofficialDcpOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation combines DEC and CMP
        let shift_value = DecOperation.modify(cpu, value);
        CmpOperation.read(cpu, shift_value);

        shift_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialDcp
    }
}

struct UnofficialIscOperation;
impl ModifyOperation for UnofficialIscOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation combines INC and SBC
        let shift_value = IncOperation.modify(cpu, value);
        SbcOperation.read(cpu, shift_value);

        shift_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialIsc
    }
}

struct UnofficialLaxOperation;
impl ReadOperation for UnofficialLaxOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        // This operation combines LDA and LDX
        LdaOperation.read(cpu, value);
        LdxOperation.read(cpu, value);
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialLax
    }
}

struct UnofficialNopOperation;

impl ImpliedOperation for UnofficialNopOperation {
    fn operate(&self, _cpu: &Cpu) { }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialNop
    }
}

// NOTE: Some undocumented opcodes do nothing, but still perform a memory read
impl ReadOperation for UnofficialNopOperation {
    fn read(&self, _cpu: &Cpu, _value: u8) { }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialNop
    }
}

struct UnofficialRlaOperation;
impl ModifyOperation for UnofficialRlaOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation combines ROL and AND
        let shift_value = RolOperation.modify(cpu, value);
        AndOperation.read(cpu, shift_value);

        shift_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialRla
    }
}

struct UnofficialRraOperation;
impl ModifyOperation for UnofficialRraOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation combines ROR and ADC
        let shift_value = RorOperation.modify(cpu, value);
        AdcOperation.read(cpu, shift_value);

        shift_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialRra
    }
}

struct UnofficialSaxOperation;
impl WriteOperation for UnofficialSaxOperation {
    fn write(&self, cpu: &Cpu) -> u8 {
        // This operation writes the bitwise-AND of A and X
        cpu.a.get() & cpu.x.get()
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialSax
    }
}

struct UnofficialSbcOperation;
impl ReadOperation for UnofficialSbcOperation {
    fn read(&self, cpu: &Cpu, value: u8) {
        SbcOperation.read(cpu, value);
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialSbc
    }
}

struct UnofficialShxOperation;
impl ModifyOperation for UnofficialShxOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation writes (X & ((value << 7) + 1))
        cpu.x.get() & ((value << 7) + 1)
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialShx
    }
}

struct UnofficialShyOperation;
impl ModifyOperation for UnofficialShyOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation writes (Y & ((value << 7) + 1))
        cpu.y.get() & ((value << 7) + 1)
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialShy
    }
}

struct UnofficialSloOperation;
impl ModifyOperation for UnofficialSloOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation combines ASL and ORA
        let shift_value = AslOperation.modify(cpu, value);
        OraOperation.read(cpu, shift_value);

        shift_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialSlo
    }
}

struct UnofficialSreOperation;
impl ModifyOperation for UnofficialSreOperation {
    fn modify(&self, cpu: &Cpu, value: u8) -> u8 {
        // This operation combines LSR and EOR
        let shift_value = LsrOperation.modify(cpu, value);
        EorOperation.read(cpu, shift_value);

        shift_value
    }

    fn instruction(&self) -> Instruction {
        Instruction::UnofficialSre
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
