use std::u8;
use std::cell::Cell;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use crate::rom::Rom;
use crate::video::Video;
use cpu::{Cpu, CpuStep};
use ppu::{Ppu, PpuStep};

pub mod cpu;
pub mod ppu;

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
            0x4016 => {
                // TODO: Return joystick state
                0x40
            }
            0x4017 => {
                // TODO: Return joystick state
                0x40
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
            0x4000..=0x4007 => {
                // TODO: APU pulse
            }
            0x4008..=0x400B => {
                // TODO: APU triangle
            }
            0x4014 => {
                self.copy_oam_dma(value);
            }
            0x4015 => {
                // TODO: APU sound channel control
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

    pub fn run<'a>(&'a self, video: &'a mut impl Video)
        -> impl Generator<Yield = NesStep, Return = !> + 'a
    {
        let mut run_cpu = Cpu::run(&self);

        let mut run_ppu = Ppu::run(&self, video);

        move || loop {
            // TODO: Clean this up
            let GeneratorState::Yielded(cpu_step) = Pin::new(&mut run_cpu).resume();
            yield NesStep::Cpu(cpu_step);

            for _ in 0u8..3 {
                loop {
                    match Pin::new(&mut run_ppu).resume() {
                        GeneratorState::Yielded(ppu_step @ PpuStep::Cycle) => {
                            yield NesStep::Ppu(ppu_step);
                            break;
                        }
                        GeneratorState::Yielded(ppu_step) => {
                            yield NesStep::Ppu(ppu_step);
                        }
                    }
                }
            }
        }
    }
}

pub enum NesStep {
    Cpu(CpuStep),
    Ppu(PpuStep),
}
