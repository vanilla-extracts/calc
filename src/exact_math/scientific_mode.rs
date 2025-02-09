use std::{f64, fmt::Display};

use crate::parsing::ast::int_to_superscript_string;

pub struct ScienceFloat {
    exponent: i32,
    value: f64, //value * 10^exponent
}

impl Display for ScienceFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.exponent {
            0 => write!(f, "{:.10}", self.value),
            _ => write!(
                f,
                "{:.10}*10{}",
                self.value,
                int_to_superscript_string(self.exponent.into())
            ),
        }
    }
}

pub fn from_float(f: f64) -> ScienceFloat {
    let multiple;
    let working_value;
    if f < 0.0 {
        multiple = -1.0;
        working_value = -f;
    } else {
        multiple = 1.0;
        working_value = f;
    }
    let exponent = working_value.log10().floor() as i32;
    let value = working_value / 10.0_f64.powi(exponent);
    ScienceFloat {
        exponent,
        value: multiple * value,
    }
}
