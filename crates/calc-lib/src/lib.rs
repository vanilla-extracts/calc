use std::cell::RefCell;

use crate::exact_math::float_mode::FloatMode;

mod configuration;
mod exact_math;
mod functions;
mod lexing;
mod parsing;
mod utils;
static VERSION: &str = "v4.0.4-alpha";
thread_local! {static FLOAT_MODE: RefCell<FloatMode> = const {RefCell::new(FloatMode::Exact)}}
