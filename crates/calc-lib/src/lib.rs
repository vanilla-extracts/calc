use std::cell::RefCell;

use crate::exact_math::float_mode::FloatMode;

pub mod configuration;
pub mod exact_math;
pub mod functions;
pub mod lexing;
pub mod parsing;
pub mod utils;
pub static VERSION: &str = "v4.0.4-alpha";
thread_local! {pub static FLOAT_MODE: RefCell<FloatMode> = const {RefCell::new(FloatMode::Exact)}}
