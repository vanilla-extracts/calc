use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;
use std::collections::HashMap;

pub fn expo(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(v)) => Parameters::Int(v),
        (Parameters::Null, Parameters::Float(f)) => Parameters::Float(f),
        (Parameters::Int(v), Parameters::Null) => Parameters::Int(v),
        (Parameters::Float(f), Parameters::Null) => Parameters::Float(f),
        (Parameters::Int(v), Parameters::Int(v2)) => Parameters::Float((v as f64).powf(v2 as f64)),
        (Parameters::Int(v), Parameters::Float(f)) => Parameters::Float((v as f64).powf(f)),
        (Parameters::Float(v), Parameters::Float(f)) => Parameters::Float(v.powf(f)),
        (Parameters::Float(v), Parameters::Int(i1)) => Parameters::Float(v.powf(i1 as f64)),

        (Parameters::Rational(s), Parameters::Null) => Parameters::Rational(s.clone()),
        (Parameters::Null, Parameters::Rational(s)) => Parameters::Rational(s.clone()),
        (Parameters::Rational(s), Parameters::Rational(s2)) => {
            Parameters::Float(s.approx().powf(s2.approx()))
        }
        (Parameters::Rational(s), Parameters::Int(i)) => {
            Parameters::Float(s.approx().powf(i as f64))
        }
        (Parameters::Int(i), Parameters::Rational(s)) => {
            Parameters::Float((i as f64).powf(s.approx()))
        }
        (Parameters::Rational(s), Parameters::Float(f)) => Parameters::Float(s.approx().powf(f)),
        (Parameters::Float(f), Parameters::Rational(s)) => Parameters::Float(f.powf(s.approx())),
        (Bool(_), Parameters::Int(i)) => Parameters::Int(i),
        (Bool(_), Parameters::Float(i)) => Parameters::Float(i),
        (Parameters::Int(i), Bool(_)) => Parameters::Int(i),
        (Parameters::Float(i), Bool(_)) => Parameters::Float(i),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => match ram {
            None => Parameters::Identifier(s),
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::Identifier(s2),
                ram,
                expo,
            ),
        },
        (Parameters::Identifier(s), Parameters::Int(i)) => match ram {
            None => Parameters::Int(i),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, expo),
        },
        (Parameters::Int(i), Parameters::Identifier(s)) => match ram {
            None => Parameters::Int(i),
            Some(_) => {
                apply_operator_reverse(Parameters::Int(i), Parameters::Identifier(s), ram, expo)
            }
        },
        (Parameters::Identifier(s), Parameters::Float(i)) => match ram {
            None => Parameters::Float(i),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, expo),
        },

        (Parameters::Rational(s), Parameters::Identifier(ss)) => match ram {
            None => Parameters::Rational(s),
            Some(_) => apply_operator_reverse(
                Parameters::Rational(s.clone()),
                Parameters::Identifier(ss.clone()),
                ram,
                expo,
            ),
        },
        (Parameters::Identifier(ss), Parameters::Rational(s)) => match ram {
            None => Rational(s),
            Some(_) => apply_operator(
                Parameters::Identifier(ss),
                Parameters::Rational(s),
                ram,
                expo,
            ),
        },
        (Parameters::Identifier(s), Parameters::Null) => match ram {
            None => Parameters::Null,
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, expo),
        },
        (Parameters::Null, Parameters::Identifier(s)) => match ram {
            None => Null,
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, expo),
        },
        (Parameters::Float(i), Parameters::Identifier(s)) => match ram {
            None => Float(i),
            Some(_) => {
                apply_operator_reverse(Parameters::Float(i), Parameters::Identifier(s), ram, expo)
            }
        },
        (Bool(b), Parameters::Identifier(s)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, expo),
        },
        (Parameters::Identifier(s), Bool(b)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator(Parameters::Identifier(s), Bool(b), ram, expo),
        },

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the ^ operator".to_string(),
        ),
    }
}
