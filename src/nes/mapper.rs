use std::cell::Cell;
use crate::nes::Nes;
use crate::rom::Rom;

#[derive(Clone)]
pub struct Mapper {
    pub rom: Rom,
    work_ram: Cell<[u8; 0x2000]>,
    chr_ram: Vec<Cell<u8>>,
}

impl Mapper {
    pub fn from_rom(rom: Rom) -> Self {
        match rom.header.mapper {
            0 => { }
            mapper => {
                unimplemented!("Mapper number {}", mapper);
            }
        }

        let work_ram = Cell::new([0; 0x2000]);
        let chr_ram = vec![Cell::new(0); rom.header.chr_ram_size_bytes];

        Mapper { rom, work_ram, chr_ram }
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

    pub fn read_ppu_u8(&self, nes: &Nes, addr: u16) -> u8 {
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

    pub fn write_ppu_u8(&self, nes: &Nes, addr: u16, value: u8) {
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
