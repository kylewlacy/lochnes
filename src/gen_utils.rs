#[macro_export]
macro_rules! yield_all {
    ($gen: expr) => {
        {
            use std::ops::{Generator, GeneratorState};
            use std::pin::Pin;

            let mut gen = $gen;
            loop {
                match Generator::resume(Pin::new(&mut gen)) {
                    GeneratorState::Yielded(yielded) => {
                        yield yielded;
                    }
                    GeneratorState::Complete(result) => {
                        break result;
                    }
                }
            }
        }
    }
}
