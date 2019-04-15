use std::cell::Cell;
use crate::rom::Rom;

#[derive(Clone)]
pub struct Mapper {
    pub rom: Rom,
    work_ram: Cell<[u8; 0x2000]>,
}

impl Mapper {
    pub fn from_rom(rom: Rom) -> Self {
        let work_ram = Cell::new([0; 0x2000]);

        Mapper { rom, work_ram }
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

    fn work_ram(&self) -> &[Cell<u8>] {
        let work_ram: &Cell<[u8]> = &self.work_ram;
        work_ram.as_slice_of_cells()
    }
}
