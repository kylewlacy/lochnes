#![feature(nll, proc_macro_non_items, use_extern_macros, generators, never_type)]

#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate enum_kinds;
extern crate structopt;

use std::path::PathBuf;
use std::process;
use std::io;
use std::fs;
use structopt::StructOpt;

mod rom;
mod nes;

fn main() {
    let opts = Options::from_args();
    match run(opts) {
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
    let mut nes = nes::Nes::new_from_rom(rom);

    loop {
        println!("{:X?}", nes.cpu);
        nes.step();
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
