use std::fmt;
use rom::Rom;

pub struct Nes {
    pub rom: Rom,
    pub ram: [u8; 0x0800],
    pub cpu: Cpu,
}

impl Nes {
    pub fn new_from_rom(rom: Rom) -> Self {
        let ram = [0; 0x0800];
        let cpu = Cpu::new();

        let mut nes = Nes {
            rom,
            ram,
            cpu,
        };

        let reset_addr = nes.read_u16(0xFFFC);

        nes.cpu.pc = reset_addr;

        nes
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        let mapper = self.rom.header.mapper;
        if mapper != 0 {
            unimplemented!("Unhandled mapper: {}", mapper);
        }

        match addr {
            0x8000...0xFFFF => {
                let rom_offset = addr - 0x8000;
                let mapped_addr = rom_offset as usize % self.rom.prg_rom.len();
                self.rom.prg_rom[mapped_addr]
            }
            _ => {
                unimplemented!("Unhandled read from address: 0x{:X}", addr);
            }
        }
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read_u8(addr);
        let hi = self.read_u8(addr + 1);

        lo as u16 | ((hi as u16) << 8)
    }

    pub fn write_u8(&mut self, addr: u16, value: u8) {
        let mapper = self.rom.header.mapper;
        if mapper != 0 {
            unimplemented!("Unhandled mapper: {}", mapper);
        }

        match addr {
            0x8000...0xFFFF => {
                let rom_offset = addr - 0x8000;
                let mapped_addr = rom_offset as usize % self.rom.prg_rom.len();
                self.rom.prg_rom[mapped_addr] = value;
            }
            _ => {
                unimplemented!("Unhandled write to address: 0x{:X}", addr);
            }
        }
    }

    pub fn step(&mut self) -> CpuStep {
        let pc = self.cpu.pc;
        let next_pc;

        let opcode = self.read_u8(pc);
        let opcode = Opcode::from_u8(opcode);

        let op;
        match opcode {
            Opcode::Cld => {
                self.cpu.set_flags(CpuFlags::D, false);
                next_pc = pc + 1;
                op = Op::Cld;
            }
            Opcode::LdaAbs => {
                let addr = self.read_u16(pc + 1);
                let value = self.read_u8(addr);
                self.cpu.a = value;
                next_pc = pc + 3;
                op = Op::LdaAbs { addr };
            }
            Opcode::LdaImm => {
                let value = self.read_u8(pc + 1);
                self.cpu.a = value;
                next_pc = pc + 2;
                op = Op::LdaImm { value };
            }
            Opcode::LdxImm => {
                let value = self.read_u8(pc + 1);
                self.cpu.x = value;
                next_pc = pc + 2;
                op = Op::LdxImm { value };
            }
            Opcode::Sei => {
                self.cpu.set_flags(CpuFlags::I, true);
                next_pc = pc + 1;
                op = Op::Sei;
            }
            Opcode::StaAbs => {
                let a = self.cpu.a;
                let addr = self.read_u16(pc + 1);
                self.write_u8(addr, a);
                next_pc = pc + 3;
                op = Op::StaAbs { addr };
            }
            Opcode::Txs => {
                let x = self.cpu.x;
                self.cpu.s = x;
                next_pc = pc + 1;
                op = Op::Txs;
            }
        }

        self.cpu.pc = next_pc;

        CpuStep { pc, op }
    }
}

#[derive(Debug)]
pub struct Cpu {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub s: u8,
    pub p: CpuFlags,
}

impl Cpu {
    fn new() -> Self {
        Cpu {
            pc: 0,
            a: 0,
            x: 0,
            y: 0,
            s: 0xFD,
            p: CpuFlags::from_bits_truncate(0x34),
        }
    }

    fn set_flags(&mut self, flags: CpuFlags, value: bool) {
        // TODO: Prevent the break (`B`) and unused (`U`) flags
        // from being changed!
        self.p.set(flags, value);
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
    Cld,
    LdaAbs { addr: u16 },
    LdaImm { value: u8 },
    LdxImm { value: u8 },
    Sei,
    StaAbs { addr: u16 },
    Txs,
}

impl Opcode {
    fn from_u8(opcode: u8) -> Self {
        match opcode {
            0x78 => {
                Opcode::Sei
            }
            0x8D => {
                Opcode::StaAbs
            }
            0x9A => {
                Opcode::Txs
            }
            0xA2 => {
                Opcode::LdxImm
            }
            0xA9 => {
                Opcode::LdaImm
            }
            0xAD => {
                Opcode::LdaAbs
            }
            0xD8 => {
                Opcode::Cld
            }
            opcode => {
                unimplemented!("Unhandled opcode: 0x{:X}", opcode);
            }
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mnemonic = match self {
            Opcode::Cld => "CLD",
            Opcode::LdaAbs | Opcode::LdaImm => "LDA",
            Opcode::LdxImm => "LDX",
            Opcode::Sei => "SEI",
            Opcode::StaAbs => "STA",
            Opcode::Txs => "TXS",
        };
        write!(f, "{}", mnemonic)?;
        Ok(())
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let opcode = Opcode::from(self);
        match self {
            Op::Cld | Op::Sei | Op::Txs => {
                write!(f, "{}", opcode)?;
            }
            Op::LdaAbs { addr } | Op::StaAbs { addr } => {
                write!(f, "{} ${:04X}", opcode, addr)?;
            }
            Op::LdaImm { value } | Op::LdxImm { value } => {
                write!(f, "{} #${:02X}", opcode, value)?;
            }
        }
        Ok(())
    }
}

pub struct CpuStep {
    pub pc: u16,
    pub op: Op,
}