#![feature(
    as_cell, cell_update, never_type, exhaustive_patterns,
    generators, generator_trait
)]
#![cfg_attr(test, feature(test))]

#[cfg(test)] extern crate test;

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::time::{Duration, Instant};
use std::path::PathBuf;
use std::process;
use std::io;
use std::fs;
use std::thread;
use structopt::StructOpt;
use sdl2::event::Event as SdlEvent;
use sdl2::keyboard::Keycode as SdlKeycode;
use nes::NesStep;
use nes::ppu::PpuStep;

#[macro_use] mod gen_utils;
mod rom;
mod nes;
mod video;

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

    #[structopt(long = "scale")]
    scale: Option<u32>,
}

fn run(opts: Options) -> Result<(), LochnesError> {
    const NES_REFRESH_RATE: Duration = Duration::from_nanos(1_000_000_000 / 60);
    const NES_WIDTH: u32 = 256;
    const NES_HEIGHT: u32 = 240;

    let bytes = fs::read(opts.rom)?;
    let rom = rom::Rom::from_bytes(bytes.into_iter())?;
    let nes = nes::Nes::new_from_rom(rom);
    let scale = opts.scale.unwrap_or(1);

    let window_width = NES_WIDTH * scale;
    let window_height = NES_HEIGHT * scale;

    let sdl = sdl2::init().map_err(LochnesError::Sdl2Error)?;
    let sdl_video = sdl.video().map_err(LochnesError::Sdl2Error)?;
    let sdl_window = sdl_video.window("Lochnes", window_width, window_height)
        .opengl()
        .build()?;
    let mut sdl_canvas = sdl_window.into_canvas()
        .build()?;
    let sdl_texture_creator = sdl_canvas.texture_creator();
    let mut sdl_event_pump = sdl.event_pump().map_err(LochnesError::Sdl2Error)?;

    let texture_buffer_video = video::TextureBufferedVideo::new(
        &sdl_texture_creator,
        NES_WIDTH,
        NES_HEIGHT
    )?;
    let mut video = &texture_buffer_video;
    let mut run_nes = nes.run(&mut video);

    'running: loop {
        let frame_start = Instant::now();
        for event in sdl_event_pump.poll_iter() {
            match event {
                SdlEvent::Quit { .. }
                | SdlEvent::KeyDown {
                    keycode: Some(SdlKeycode::Escape), ..
                } => {
                    break 'running;
                }
                _ => { }
            }
        }

        loop {
            match Pin::new(&mut run_nes).resume() {
                GeneratorState::Yielded(NesStep::Ppu(PpuStep::Vblank)) => {
                    break;
                }
                // GeneratorState::Yielded(NesStep::Cpu(nes::cpu::CpuStep::Op(op))) => {
                //     println!("{:X?}", nes.cpu);
                //     println!("${:04X}: {}", op.pc, op.op);
                //     println!();
                // }
                GeneratorState::Yielded(_) => { }
            }
        }

        texture_buffer_video.copy_to(&mut sdl_canvas)
            .map_err(LochnesError::Sdl2Error)?;
        sdl_canvas.present();

        let elapsed = frame_start.elapsed();
        println!("frame time: {:5.2}ms", elapsed.as_micros() as f64 / 1_000.0);
        let duration_until_refresh = NES_REFRESH_RATE.checked_sub(elapsed);
        let sleep_duration = duration_until_refresh.unwrap_or_else(|| {
            Duration::from_secs(0)
        });
        thread::sleep(sleep_duration);
    }

    Ok(())
}

#[derive(Debug)]
enum LochnesError {
    IoError(io::Error),
    RomError(rom::RomError),
    Sdl2Error(String),
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

impl From<sdl2::video::WindowBuildError> for LochnesError {
    fn from(err: sdl2::video::WindowBuildError) -> Self {
        LochnesError::Sdl2Error(err.to_string())
    }
}

impl From<sdl2::IntegerOrSdlError> for LochnesError {
    fn from(err: sdl2::IntegerOrSdlError) -> Self {
        LochnesError::Sdl2Error(err.to_string())
    }
}

impl From<sdl2::render::TextureValueError> for LochnesError {
    fn from(err: sdl2::render::TextureValueError) -> Self {
        LochnesError::Sdl2Error(err.to_string())
    }
}



#[cfg(test)]
mod tests {
    use std::{env, fs};
    use test::Bencher;
    use nes::cpu::CpuStep;
    use super::*;

    #[bench]
    fn bench_cycles(b: &mut Bencher) {
        // TODO: Add a ROM as a fixture for benchmarking
        let rom_path = env::var("BENCH_ROM").expect("BENCH_ROM env var must be set for benchmarking");
        let rom_bytes = fs::read(rom_path).expect("Failed to open BENCH_ROM");
        let rom = rom::Rom::from_bytes(rom_bytes.into_iter()).expect("Failed to parse BENCH_ROM into a valid ROM");

        let cycles = env::var("BENCH_CYCLES").expect("BENCH_CYCLES env var must be set for benchmarking");
        let cycles: u64 = cycles.parse().unwrap();

        b.iter(|| {
            let nes = nes::Nes::new_from_rom(rom.clone());
            let mut video = video::NullVideo;
            let mut run_nes = nes.run(&mut video);

            for _ in 0..cycles {
                loop {
                    match Pin::new(&mut run_nes).resume() {
                        GeneratorState::Yielded(NesStep::Cpu(CpuStep::Cycle)) => { break; }
                        GeneratorState::Yielded(_) => { }
                    }
                }
            }
        });
    }
}
