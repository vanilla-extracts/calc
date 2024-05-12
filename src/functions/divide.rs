use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;
use std::collections::HashMap;

pub fn divide(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(v)) => Parameters::Int(v),
        (Parameters::Null, Parameters::Float(f)) => Parameters::Float(f),
        (Parameters::Int(v), Parameters::Null) => Parameters::Int(v),
        (Parameters::Float(f), Parameters::Null) => Parameters::Float(f),
        (Parameters::Int(v), Parameters::Int(v2)) => Parameters::Rational(Rationals::new(v2, v)),
        (Parameters::Int(v), Parameters::Float(f)) => Parameters::Float((v as f64) / f),
        (Parameters::Float(v), Parameters::Float(f)) => Parameters::Float(v / f),
        (Parameters::Float(v), Parameters::Int(i1)) => Parameters::Float(v / (i1 as f64)),
        (Parameters::Null, Parameters::InterpreterVector(vec)) => {
            Parameters::InterpreterVector(vec.clone())
        }
        (Parameters::InterpreterVector(vec), Parameters::Null) => {
            Parameters::InterpreterVector(vec.clone())
        }

        (Parameters::Rational(s), Parameters::Null) => {
            Parameters::Rational(Rationals::new(1, 1) / s)
        }
        (Parameters::Null, Parameters::Rational(s)) => {
            Parameters::Rational(Rationals::new(1, 1) / s)
        }
        (Parameters::Rational(s), Parameters::Rational(s2)) => Parameters::Rational(s / s2),

        (Parameters::Rational(s), Parameters::Int(i)) => {
            Parameters::Rational(s / Rationals::new(1, i))
        }
        (Parameters::Int(i), Parameters::Rational(s)) => {
            Parameters::Rational(Rationals::new(1, i) / s)
        }
        (Parameters::Rational(s), Parameters::Float(f)) => Parameters::Float(s.approx() / f),
        (Parameters::Float(f), Parameters::Rational(s)) => Parameters::Float(f / s.approx()),
        (Bool(_), Parameters::Int(i)) => Parameters::Int(i),
        (Bool(_), Parameters::Float(i)) => Parameters::Float(i),
        (Parameters::Int(i), Bool(_)) => Parameters::Int(i),
        (Parameters::Float(i), Bool(_)) => Parameters::Float(i),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => match ram {
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::Identifier(s2),
                ram,
                divide,
            ),
            None => Parameters::Div(
                Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
                Box::from(Parameters::Var(
                    Box::from(Parameters::Int(1)),
                    1,
                    s2.clone(),
                )),
            ),
        },

        (Parameters::Rational(s), Parameters::Identifier(ss)) => match ram {
            Some(_) => apply_operator_reverse(
                Parameters::Rational(s.clone()),
                Parameters::Identifier(ss.clone()),
                ram,
                divide,
            ),
            None => Parameters::Div(
                Box::from(Parameters::Int(s.over)),
                Box::from(Parameters::Var(
                    Box::from(Parameters::Int(s.under)),
                    1,
                    ss.clone(),
                )),
            ),
        },
        (Parameters::Identifier(ss), Parameters::Rational(s)) => match ram {
            Some(_) => apply_operator(
                Parameters::Identifier(ss),
                Parameters::Rational(s),
                ram,
                divide,
            ),
            None => match s.invert() {
                Ok(r) => Parameters::Var(Box::from(Parameters::Rational(r)), 1, ss.clone()),
                Err(_) => Parameters::Null,
            },
        },

        (Parameters::Identifier(s), Parameters::Int(i)) => match ram {
            None => Parameters::Var(
                Box::new(Parameters::Rational(Rationals::new(i, 1))),
                1,
                s.clone(),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, divide),
        },
        (Parameters::Int(i), Parameters::Identifier(s)) => match ram {
            None => Parameters::Div(
                Box::from(Parameters::Int(i)),
                Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
            ),
            Some(_) => {
                let v = apply_operator(
                    Parameters::Identifier(s.clone()),
                    Parameters::Int(i),
                    ram,
                    divide,
                );
                match v {
                    Parameters::Float(i) => Parameters::Float(1.0 / i),
                    _ => divide(Parameters::Int(i), Parameters::Identifier(s.clone()), None),
                }
            }
        },
        (Parameters::Null, Parameters::Identifier(s)) => match ram {
            None => Parameters::Div(
                Box::new(Parameters::Int(1)),
                Box::new(Parameters::Var(Box::new(Parameters::Int(1)), 1, s.clone())),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, divide),
        },
        (Parameters::Identifier(s), Parameters::Null) => match ram {
            None => Parameters::Div(
                Box::new(Parameters::Int(1)),
                Box::new(Parameters::Var(Box::new(Parameters::Int(1)), 1, s.clone())),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, divide),
        },
        (Parameters::Identifier(s), Parameters::Float(i)) => match ram {
            None => Parameters::Var(
                Box::from(Parameters::Rational(Rationals::rationalize(1.0 / i))),
                1,
                s.clone(),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, divide),
        },
        (Parameters::Float(i), Parameters::Identifier(s)) => match ram {
            None => Parameters::Div(
                Box::from(Parameters::Int(Rationals::rationalize(i).over)),
                Box::from(Parameters::Var(
                    Box::from(Parameters::Int(Rationals::rationalize(i).under)),
                    1,
                    s.clone(),
                )),
            ),
            Some(_) => {
                let v =
                    apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, divide);
                match v {
                    Parameters::Float(i) => Parameters::Float(1.0 / i),
                    _ => Parameters::Null,
                }
            }
        },
        (Bool(b), Parameters::Identifier(s)) => match ram {
            None => Parameters::Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, divide),
        },
        (Parameters::Identifier(s), Bool(b)) => match ram {
            None => Parameters::Bool(b),
            Some(_) => apply_operator(Parameters::Identifier(s), Bool(b), ram, divide),
        },
        _ => Parameters::Identifier(
            "@Those two values are incompatible with the / operator".to_string(),
        ),
    }
}
