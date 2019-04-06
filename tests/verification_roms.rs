#![feature(generator_trait, exhaustive_patterns)]

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use lochnes::{nes, rom, video};
use lochnes::nes::NesStep;
use lochnes::nes::ppu::PpuStep;

const ROM_BLARGG_EXPECTED_OUTPUT: &str = r##"
01-implied

Passed
"##;

#[test]
fn rom_blargg_instr_test() {
    let rom_bytes = include_bytes!("./fixtures/nes-test-roms/nes_instr_test/rom_singles/01-implied.nes");
    let rom = rom::Rom::from_bytes(rom_bytes.into_iter().cloned())
        .expect("Failed to parse test ROM");

    let nes = nes::Nes::new_from_rom(rom.clone());
    let mut video = video::NullVideo;
    let mut run_nes = nes.run(&mut video);

    // Run for a max of 60 frames, just in case the test ROM never completes
    for frame in 0..60 {
        loop {
            match Pin::new(&mut run_nes).resume() {
                GeneratorState::Yielded(NesStep::Ppu(PpuStep::Vblank)) => { break; }
                GeneratorState::Yielded(_) => { }
            }
        }

        const STATUS_TEST_IS_RUNNING: u8 = 0x80;
        const STATUS_TEST_NEEDS_RESET: u8 = 0x81;

        let status = nes.read_u8(0x6000);
        match (status, frame) {
            (_, 0) => { } // Ignore status on first frame
            (STATUS_TEST_IS_RUNNING, _) => { }
            (STATUS_TEST_NEEDS_RESET, _) => {
                unimplemented!("Verification ROM requested a reset!");
            }
            _ => { break; }
        }
    }

    let status = nes.read_u8(0x6000);

    let mut test_output = vec![];
    for i in 0x6004_u16.. {
        let byte = nes.read_u8(i);
        match byte {
            0 => { break; }
            byte => { test_output.push(byte); }
        }
    }

    let test_output = String::from_utf8_lossy(&test_output);
    println!("{}", test_output);
    assert_eq!(status, 0);
    assert_eq!(test_output, ROM_BLARGG_EXPECTED_OUTPUT);
}
