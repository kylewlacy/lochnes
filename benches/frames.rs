#![feature(test, generator_trait, exhaustive_patterns)]

extern crate test;

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::{env, fs};
use test::Bencher;

use lochnes::{nes, rom, video};
use lochnes::nes::NesStep;
use lochnes::nes::ppu::PpuStep;

#[bench]
fn bench_frames(b: &mut Bencher) {
    // TODO: Add a ROM as a fixture for benchmarking
    let rom_path = env::var("BENCH_ROM").expect("BENCH_ROM env var must be set for benchmarking");
    let rom_bytes = fs::read(rom_path).expect("Failed to open BENCH_ROM");
    let rom = rom::Rom::from_bytes(rom_bytes.into_iter()).expect("Failed to parse BENCH_ROM into a valid ROM");

    b.iter(|| {
        let nes = nes::Nes::new_from_rom(rom.clone());
        let video = video::NullVideo;
        let mut run_nes = nes.run(video);

        for _ in 0..10 {
            loop {
                match Pin::new(&mut run_nes).resume() {
                    GeneratorState::Yielded(NesStep::Ppu(PpuStep::Vblank)) => { break; }
                    GeneratorState::Yielded(_) => { }
                }
            }
        }
    });
}
