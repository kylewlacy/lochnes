#[macro_export]
macro_rules! yield_all {
    ($gen: expr) => {{
        use std::ops::{Generator, GeneratorState};
        use std::pin::Pin;

        let mut gen = $gen;
        loop {
            match Pin::new(&mut gen).resume(()) {
                GeneratorState::Yielded(yielded) => {
                    yield yielded;
                }
                GeneratorState::Complete(result) => {
                    break result;
                }
            }
        }
    }};
}
