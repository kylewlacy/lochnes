#![feature(
    cell_update, never_type, exhaustive_patterns,
    generators, generator_trait
)]

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::time::{Duration, Instant};
use std::path::PathBuf;
use std::process;
use std::io;
use std::fs;
use std::thread;
use log::{info, debug, trace};
use structopt::StructOpt;
use sdl2::event::Event as SdlEvent;
use sdl2::keyboard::Keycode as SdlKeycode;
use sdl2::controller::Button as SdlButton;
use nes::NesStep;
use nes::ppu::PpuStep;

use lochnes::{rom, nes, video, input};

fn main() {
    let opts = Options::from_args();
    stderrlog::new()
        .module(module_path!())
        .quiet(opts.quiet || opts.verbose == 6)
        .verbosity(opts.verbose as usize)
        .init()
        .expect("Failed to set up logging");
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

    #[structopt(short = "q", long = "quiet")]
    quiet: bool,

    #[structopt(short = "v", parse(from_occurrences))]
    verbose: u8,
}

fn run(opts: Options) -> Result<(), LochnesError> {
    debug!("Options: {:#?}", opts);

    #[cfg(feature = "easter-egg")] {
        if opts.verbose == 6 {
            let bytes = include_bytes!("../tests/fixtures/egg.nes");
            let mut bytes: Vec<u8> = bytes.to_vec();
            let nmi = &mut bytes[0x400C];
            *nmi = nmi.wrapping_add(opts.verbose / 2);
            let rom = rom::Rom::from_bytes(bytes.into_iter())?;
            run_rom(opts, rom)?;
            return Ok(());
        }
    }

    let bytes = fs::read(&opts.rom)?;
    let rom = rom::Rom::from_bytes(bytes.into_iter())?;

    debug!("ROM header: {:#04X?}", rom.header);

    run_rom(opts, rom)?;

    Ok(())
}

fn run_rom(opts: Options, rom: rom::Rom) -> Result<(), LochnesError> {
    const NES_REFRESH_RATE: Duration = Duration::from_nanos(1_000_000_000 / 60);
    const NES_WIDTH: u32 = 256;
    const NES_HEIGHT: u32 = 240;

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
    let sdl_controllers = sdl.game_controller()
        .map_err(LochnesError::Sdl2Error)?;

    let num_sdl_controllers = sdl_controllers.num_joysticks()
        .map_err(LochnesError::Sdl2Error)?;

    let sdl_controller_index = (0..num_sdl_controllers).find_map(|n| {
        if sdl_controllers.is_game_controller(n) {
            Some(n)
        }
        else {
            None
        }
    });
    let _sdl_controller = sdl_controller_index.map(|index| {
        sdl_controllers.open(index)
    }).transpose()?;

    let mut sdl_event_pump = sdl.event_pump().map_err(LochnesError::Sdl2Error)?;

    let video = &video::TextureBufferedVideo::new(
        &sdl_texture_creator,
        NES_WIDTH,
        NES_HEIGHT
    )?;
    let mut input_state = input::InputState::default();
    let input = &input::SampledInput::new(input_state);
    let io = nes::NesIoWith { video, input };
    let nes = nes::Nes::new(&io, rom);
    let mut run_nes = nes.run();

    'running: loop {
        let frame_start = Instant::now();
        for event in sdl_event_pump.poll_iter() {
            match event {
                SdlEvent::Quit { .. }
                | SdlEvent::KeyDown { keycode: Some(SdlKeycode::Escape), .. }
                    =>
                {
                    break 'running;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Z), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::A, .. }
                    =>
                {
                    input_state.joypad_1.a = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Z), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::A, .. }
                    =>
                {
                    input_state.joypad_1.a = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::X), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::B, .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::X, .. }
                    =>
                {
                    input_state.joypad_1.b = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::X), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::B, .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::X, .. }
                    =>
                {
                    input_state.joypad_1.b = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Return), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::Start, .. }
                    =>
                {
                    input_state.joypad_1.start = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Return), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::Start, .. }
                    =>
                {
                    input_state.joypad_1.start = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Backslash), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::Back, .. }
                    =>
                {
                    input_state.joypad_1.select = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Backslash), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::Back, .. }
                    =>
                {
                    input_state.joypad_1.select = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Up), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::DPadUp, .. }
                    =>
                {
                    input_state.joypad_1.up = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Up), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::DPadUp, .. }
                    =>
                {
                    input_state.joypad_1.up = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Down), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::DPadDown, .. }
                    =>
                {
                    input_state.joypad_1.down = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Down), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::DPadDown, .. }
                    =>
                {
                    input_state.joypad_1.down = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Left), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::DPadLeft, .. }
                    =>
                {
                    input_state.joypad_1.left = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Left), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::DPadLeft, .. }
                    =>
                {
                    input_state.joypad_1.left = false;
                }
                SdlEvent::KeyDown { keycode: Some(SdlKeycode::Right), .. }
                | SdlEvent::ControllerButtonDown { button: SdlButton::DPadRight, .. }
                    =>
                {
                    input_state.joypad_1.right = true;
                }
                SdlEvent::KeyUp { keycode: Some(SdlKeycode::Right), .. }
                | SdlEvent::ControllerButtonUp { button: SdlButton::DPadRight, .. }
                    =>
                {
                    input_state.joypad_1.right = false;
                }
                _ => { }
            }
        }

        input.set_state(input_state);
        debug!("Input: {:?}", input_state);

        loop {
            match Pin::new(&mut run_nes).resume(()) {
                GeneratorState::Yielded(NesStep::Ppu(PpuStep::Vblank)) => {
                    break;
                }
                GeneratorState::Yielded(NesStep::Cpu(nes::cpu::CpuStep::Op(op))) => {
                    trace!("{:X?}", nes.cpu);
                    trace!("${:04X}: {}", op.pc, op.op);
                    trace!("----------");
                }
                GeneratorState::Yielded(_) => { }
            }
        }

        video.copy_to(&mut sdl_canvas).map_err(LochnesError::Sdl2Error)?;
        sdl_canvas.present();

        let elapsed = frame_start.elapsed();
        info!("frame time: {:5.2}ms", elapsed.as_micros() as f64 / 1_000.0);
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
