#[derive(Debug, Clone, Copy, Default)]
pub struct InputState {
    pub joypad_1: JoypadState,
    pub joypad_2: JoypadState,
}

#[derive(Debug, Clone, Copy)]
pub struct JoypadState {
    pub a: bool,
    pub b: bool,
    pub start: bool,
    pub select: bool,
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
}

impl Default for JoypadState {
    fn default() -> Self {
        JoypadState {
            a: false,
            b: false,
            start: false,
            select: false,
            up: false,
            down: false,
            left: false,
            right: false,
        }
    }
}

pub trait Input {
    fn input_state(&self) -> InputState;
}

pub struct NullInput;

impl Input for NullInput {
    fn input_state(&self) -> InputState {
        InputState::default()
    }
}
