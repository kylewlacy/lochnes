#![feature(
    futures_api, async_await, await_macro, as_cell, cell_update,
    generators, generator_trait, never_type, exhaustive_patterns
)]
#![cfg_attr(test, feature(test))]

#[macro_use] extern crate bitflags;
#[macro_use] extern crate enum_kinds;
#[cfg(test)] extern crate test;

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::path::PathBuf;
use std::process;
use std::io;
use std::fs;
use structopt::StructOpt;

mod rom;
mod nes;

fn main() {
    let opts = Options::from_args();
    let run_result = run(opts);

    match run_result {
        Ok(_) => { }
        Err(err) => {
            eprintln!("{:?}", err);
            process::exit(1);
        }
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "lochnes")]
struct Options {
    #[structopt(name = "ROM", parse(from_os_str))]
    rom: PathBuf,
}

fn run(opts: Options) -> Result<(), LochnesError> {
    let bytes = fs::read(opts.rom)?;
    let rom = rom::Rom::from_bytes(bytes.into_iter())?;
    let nes = nes::Nes::new_from_rom(rom);
    let mut run_nes = nes.run();

    loop {
        let GeneratorState::Yielded(step) = Pin::new(&mut run_nes).resume();

        println!("{:X?}", nes.cpu);
        println!("${:04X}: {}", step.pc, step.op);
        println!();
    }
}

#[derive(Debug)]
enum LochnesError {
    IoError(io::Error),
    RomError(rom::RomError),
}

impl From<io::Error> for LochnesError {
    fn from(err: io::Error) -> Self {
        LochnesError::IoError(err)
    }
}

impl From<rom::RomError> for LochnesError {
    fn from(err: rom::RomError) -> Self {
        LochnesError::RomError(err)
    }
}



#[cfg(test)]
mod tests {
    use std::{env, fs};
    use test::Bencher;
    use super::*;

    #[bench]
    fn bench_cycles(b: &mut Bencher) {
        // TODO: Add a ROM as a fixture for benchmarking
        let rom_path = env::var("BENCH_ROM").expect("BENCH_ROM env var must be set for benchmarking");
        let rom_bytes = fs::read(rom_path).expect("Failed to open BENCH_ROM");
        let rom = rom::Rom::from_bytes(rom_bytes.into_iter()).expect("Failed to parse BENCH_ROM into a valid ROM");

        let steps = env::var("BENCH_STEPS").expect("BENCH_STEPS env var must be set for benchmarking");
        let steps: u64 = steps.parse().unwrap();

        b.iter(|| {
            let nes = nes::Nes::new_from_rom(rom.clone());
            let mut run_nes = nes.run();

            for _ in 0..steps {
                let GeneratorState::Yielded(_) = Pin::new(&mut run_nes).resume();
            }
        });
    }
}
