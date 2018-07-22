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

    pub fn step(&mut self) {
        let pc = self.cpu.pc;

        let opcode = self.read_u8(pc);
        let opcode = Opcode::from_u8(opcode);
        match opcode {

        }
    }
}

#[derive(Debug)]
pub struct Cpu {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub s: u8,
    pub p: u8,
}

impl Cpu {
    fn new() -> Self {
        Cpu {
            pc: 0,
            a: 0,
            x: 0,
            y: 0,
            s: 0xFD,
            p: 0x34,
        }
    }
}

enum Opcode {

}

impl Opcode {
    fn from_u8(opcode: u8) -> Self {
        match opcode {
            opcode => {
                unimplemented!("Unhandled opcode: 0x{:X}", opcode);
            }
        }
    }
}
