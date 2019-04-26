use std::cell::Cell;
use crate::nes::Nes;
use crate::rom::Rom;
use crate::video::Video;

#[derive(Clone)]
pub enum Mapper {
    Nrom(NromMapper),
    Uxrom(UxromMapper),
}

impl Mapper {
    pub fn from_rom(rom: Rom) -> Self {
        match rom.header.mapper {
            0 => Mapper::Nrom(NromMapper::from_rom(rom)),
            2 => Mapper::Uxrom(UxromMapper::from_rom(rom)),
            mapper => {
                unimplemented!("Mapper number {}", mapper);
            }
        }
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        match self {
            Mapper::Nrom(mapper) => mapper.read_u8(addr),
            Mapper::Uxrom(mapper) => mapper.read_u8(addr),
        }
    }

    pub fn write_u8(&self, addr: u16, value: u8) {
        match self {
            Mapper::Nrom(mapper) => mapper.write_u8(addr, value),
            Mapper::Uxrom(mapper) => mapper.write_u8(addr, value),
        }
    }

    pub fn read_ppu_u8(&self, nes: &Nes<impl Video>, addr: u16) -> u8 {
        match self {
            Mapper::Nrom(mapper) => mapper.read_ppu_u8(nes, addr),
            Mapper::Uxrom(mapper) => mapper.read_ppu_u8(nes, addr),
        }
    }

    pub fn write_ppu_u8(&self, nes: &Nes<impl Video>, addr: u16, value: u8) {
        match self {
            Mapper::Nrom(mapper) => mapper.write_ppu_u8(nes, addr, value),
            Mapper::Uxrom(mapper) => mapper.write_ppu_u8(nes, addr, value),
        }
    }
}



#[derive(Clone)]
pub struct NromMapper {
    rom: Rom,
    work_ram: Cell<[u8; 0x2000]>,
    chr_ram: Vec<Cell<u8>>,
}

impl NromMapper {
    pub fn from_rom(rom: Rom) -> Self {
        let work_ram = Cell::new([0; 0x2000]);
        let chr_ram = vec![Cell::new(0); rom.header.chr_ram_size_bytes];

        NromMapper { rom, work_ram, chr_ram }
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        let work_ram = self.work_ram();
        let prg_rom = &self.rom.prg_rom;

        match addr {
            0x0000..=0x5FFF => {
                panic!("Tried to read from mapper at address ${:04X}", addr);
            }
            0x6000..=0x7FFF => {
                let offset = ((addr - 0x6000) as usize) % work_ram.len();
                work_ram[offset].get()
            }
            0x8000..=0xFFFF => {
                let offset = ((addr - 0x8000) as usize) % prg_rom.len();
                prg_rom[offset]
            }
        }
    }

    pub fn write_u8(&self, addr: u16, value: u8) {
        let work_ram = self.work_ram();

        match addr {
            0x0000..=0x5FFF => {
                panic!("Tried to write to mapper at address ${:04X}", addr);
            }
            0x6000..=0x7FFF => {
                let offset = ((addr - 0x6000) as usize) % work_ram.len();
                work_ram[offset].set(value);
            }
            0x8000..=0xFFFF => { }
        }
    }

    pub fn read_ppu_u8(&self, nes: &Nes<impl Video>, addr: u16) -> u8 {
        let chr_rom = &self.rom.chr_rom;
        let chr_ram = &self.chr_ram;
        let ppu_ram = nes.ppu.ppu_ram();
        match addr {
            0x0000..=0x1FFF => {
                if chr_rom.is_empty() {
                    let offset = (addr as usize) % chr_ram.len();
                    chr_ram[offset].get()
                }
                else {
                    let offset = (addr as usize) % chr_rom.len();
                    chr_rom[offset]
                }
            }
            0x2000..=0x2FFF => {
                let offset = ((addr - 0x2000) as usize) % ppu_ram.len();
                ppu_ram[offset].get()
            }
            0x3000..=0x3EFF => {
                let offset = (addr - 0x3000) % 0x0FFF;
                self.read_ppu_u8(nes, offset + 0x2000)
            }
            0x3F00..=0xFFFF => {
                unreachable!();
            }
        }
    }

    pub fn write_ppu_u8(&self, nes: &Nes<impl Video>, addr: u16, value: u8) {
        let ppu_ram = nes.ppu.ppu_ram();
        let chr_rom = &self.rom.chr_rom;
        let chr_ram = &self.chr_ram;
        match addr {
            0x0000..=0x1FFF => {
                if chr_rom.is_empty() {
                    let offset = (addr as usize) % chr_ram.len();
                    chr_ram[offset].set(value);
                }
                else {
                    // Do nothing-- tried to write to read-only CHR ROM
                }
            }
            0x2000..=0x2FFF => {
                let offset = ((addr - 0x2000) as usize) % ppu_ram.len();
                ppu_ram[offset].set(value)
            }
            0x3000..=0x3EFF => {
                let offset = (addr - 0x3000) % 0x0FFF;
                self.write_ppu_u8(nes, offset + 0x2000, value)
            }
            0x3F00..=0xFFFF => {
                unreachable!();
            }
        }
    }

    fn work_ram(&self) -> &[Cell<u8>] {
        let work_ram: &Cell<[u8]> = &self.work_ram;
        work_ram.as_slice_of_cells()
    }
}

#[derive(Clone)]
pub struct UxromMapper {
    rom: Rom,
    bank: Cell<usize>,
    work_ram: Cell<[u8; 0x2000]>,
    chr_ram: Vec<Cell<u8>>,
}

impl UxromMapper {
    pub fn from_rom(rom: Rom) -> Self {
        let work_ram = Cell::new([0; 0x2000]);
        let chr_ram = vec![Cell::new(0); rom.header.chr_ram_size_bytes];
        let bank = Cell::new(5);

        UxromMapper { rom, bank, work_ram, chr_ram }
    }

    pub fn banks<'a>(&'a self) -> impl ExactSizeIterator<Item=&'a [u8]> + 'a {
        self.rom.prg_rom.chunks(16_384)
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        let work_ram = self.work_ram();
        let mut banks = self.banks();

        match addr {
            0x0000..=0x5FFF => {
                panic!("Tried to read from mapper at address ${:04X}", addr);
            }
            0x6000..=0x7FFF => {
                let offset = ((addr - 0x6000) as usize) % work_ram.len();
                work_ram[offset].get()
            }
            0x8000..=0xBFFF => {
                let n = self.bank.get();
                let bank = banks.nth(n).unwrap();
                let offset = ((addr - 0x8000) as usize) % bank.len();
                bank[offset]
            }
            0xC000..=0xFFFF => {
                let bank = banks.last().unwrap();
                let offset = ((addr - 0xC000) as usize) % bank.len();
                bank[offset]
            }
        }
    }

    pub fn write_u8(&self, addr: u16, value: u8) {
        let work_ram = self.work_ram();
        let banks = self.banks();

        match addr {
            0x0000..=0x5FFF => {
                panic!("Tried to write to mapper at address ${:04X}", addr);
            }
            0x6000..=0x7FFF => {
                let offset = ((addr - 0x6000) as usize) % work_ram.len();
                work_ram[offset].set(value);
            }
            0x8000..=0xFFFF => {
                let new_bank = (value & 0b_0000_1111) as usize;
                if new_bank > banks.len() {
                    unimplemented!("UxROM tried to select bank by writing byte 0x{:02X}, but ROM only has {} bank(s)", value, banks.len());
                }

                self.bank.set(new_bank);
            }
        }
    }

    pub fn read_ppu_u8(&self, nes: &Nes<impl Video>, addr: u16) -> u8 {
        let chr_rom = &self.rom.chr_rom;
        let chr_ram = &self.chr_ram;
        let ppu_ram = nes.ppu.ppu_ram();
        match addr {
            0x0000..=0x1FFF => {
                if chr_rom.is_empty() {
                    let offset = (addr as usize) % chr_ram.len();
                    chr_ram[offset].get()
                }
                else {
                    let offset = (addr as usize) % chr_rom.len();
                    chr_rom[offset]
                }
            }
            0x2000..=0x2FFF => {
                let offset = ((addr - 0x2000) as usize) % ppu_ram.len();
                ppu_ram[offset].get()
            }
            0x3000..=0x3EFF => {
                let offset = (addr - 0x3000) % 0x0FFF;
                self.read_ppu_u8(nes, offset + 0x2000)
            }
            0x3F00..=0xFFFF => {
                unreachable!();
            }
        }
    }

    pub fn write_ppu_u8(&self, nes: &Nes<impl Video>, addr: u16, value: u8) {
        let ppu_ram = nes.ppu.ppu_ram();
        let chr_rom = &self.rom.chr_rom;
        let chr_ram = &self.chr_ram;
        match addr {
            0x0000..=0x1FFF => {
                if chr_rom.is_empty() {
                    let offset = (addr as usize) % chr_ram.len();
                    chr_ram[offset].set(value);
                }
                else {
                    // Do nothing-- tried to write to read-only CHR ROM
                }
            }
            0x2000..=0x2FFF => {
                let offset = ((addr - 0x2000) as usize) % ppu_ram.len();
                ppu_ram[offset].set(value)
            }
            0x3000..=0x3EFF => {
                let offset = (addr - 0x3000) % 0x0FFF;
                self.write_ppu_u8(nes, offset + 0x2000, value)
            }
            0x3F00..=0xFFFF => {
                unreachable!();
            }
        }
    }

    fn work_ram(&self) -> &[Cell<u8>] {
        let work_ram: &Cell<[u8]> = &self.work_ram;
        work_ram.as_slice_of_cells()
    }
}
