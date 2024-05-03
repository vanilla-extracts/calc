use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::add::add;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;
use crate::utils::matrix_utils::*;
use std::collections::HashMap;

pub fn mult(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(v)) => Parameters::Int(v),
        (Parameters::Null, Parameters::Float(f)) => Parameters::Float(f),
        (Parameters::Int(v), Parameters::Null) => Parameters::Int(v),
        (Parameters::Float(f), Parameters::Null) => Parameters::Float(f),
        (Parameters::Int(v), Parameters::Int(v2)) => Parameters::Int(v * v2),
        (Parameters::Int(v), Parameters::Float(f)) => Parameters::Float((v as f64) * f),
        (Parameters::Float(v), Parameters::Float(f)) => Parameters::Float(v * f),
        (Parameters::Float(v), Parameters::Int(i1)) => Parameters::Float(v * (i1 as f64)),

        (Parameters::Rational(s), Parameters::Null) => Parameters::Rational(s.clone()),
        (Parameters::Null, Parameters::Rational(s)) => Parameters::Rational(s.clone()),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Parameters::Rational(s * s2),
        (Parameters::Rational(s), Parameters::Int(i)) => {
            Parameters::Rational(s * Rationals::new(1, i))
        }
        (Parameters::Int(i), Parameters::Rational(s)) => {
            Parameters::Rational(s * Rationals::new(1, i))
        }
        (Parameters::Rational(s), Parameters::Float(f)) => Parameters::Float(s.approx() * f),
        (Parameters::Float(f), Parameters::Rational(s)) => Parameters::Float(f * s.approx()),
        (Parameters::Null, Parameters::InterpreterVector(vec)) => {
            Parameters::InterpreterVector(vec.clone())
        }
        (Parameters::InterpreterVector(vec), Parameters::Null) => {
            Parameters::InterpreterVector(vec.clone())
        }
        (Parameters::InterpreterVector(vec), Parameters::Int(v)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Parameters::Int(v), ram))
                .for_each(|x| result.push(x));
            Parameters::InterpreterVector(Box::from(result))
        }
        (Parameters::Int(v), Parameters::InterpreterVector(vec)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Parameters::Int(v), ram))
                .for_each(|x| result.push(x));
            Parameters::InterpreterVector(Box::from(result))
        }
        (Parameters::InterpreterVector(vec), Parameters::Float(v)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Parameters::Float(v), ram))
                .for_each(|x| result.push(x));
            Parameters::InterpreterVector(Box::from(result))
        }
        (Parameters::Float(v), Parameters::InterpreterVector(vec)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Parameters::Float(v), ram))
                .for_each(|x| result.push(x));
            Parameters::InterpreterVector(Box::from(result))
        }

        (Parameters::InterpreterVector(vec), Parameters::InterpreterVector(vec2)) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            let mut res2 = Vec::new();

            vec.clone().into_iter().for_each(|x| match x {
                Parameters::InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });
            vec2.clone().into_iter().for_each(|x| match x {
                Parameters::InterpreterVector(l) => res2.push(l.to_vec()),
                _ => {
                    is_matrix = false;
                }
            });

            if !is_matrix {
                let mut sum = Parameters::Null;
                (*vec)
                    .into_iter()
                    .zip(vec2.into_iter())
                    .map(|(a, b)| mult(a.clone(), b.clone(), ram))
                    .for_each(|x| sum = add(sum.clone(), x, ram));

                match sum {
                    Parameters::Int(i) => Parameters::Int(i),
                    Parameters::Float(f) => Parameters::Float(f),
                    _ => Parameters::Float(f64::NAN),
                }
            } else {
                let matrix_result = mult_matrix(res, res2, ram);

                let mut res = Vec::new();

                if matrix_result.len() == 0 {
                    return Parameters::Null;
                }

                matrix_result
                    .into_iter()
                    .for_each(|x| res.push(Parameters::InterpreterVector(Box::from(x))));

                Parameters::InterpreterVector(Box::from(res))
            }
        }

        (Bool(_), Parameters::Int(i)) => Parameters::Int(i),
        (Bool(_), Parameters::Float(i)) => Parameters::Float(i),
        (Parameters::Int(i), Bool(_)) => Parameters::Int(i),
        (Parameters::Float(i), Bool(_)) => Parameters::Float(i),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => match ram {
            None => {
                if s == s2 {
                    Parameters::Var(Box::from(Parameters::Int(1)), 2, s.clone())
                } else {
                    Parameters::Mul(
                        Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
                        Box::from(Parameters::Var(
                            Box::from(Parameters::Int(1)),
                            1,
                            s2.clone(),
                        )),
                    )
                }
            }
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::Identifier(s2),
                ram,
                mult,
            ),
        },
        (Parameters::Identifier(s), Parameters::Int(i)) => match ram {
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, mult),
            None => Parameters::Var(Box::from(Parameters::Int(i)), 1, s.clone()),
        },

        (Parameters::Rational(s), Parameters::Identifier(ss)) => match ram {
            None => Parameters::Var(Box::from(Parameters::Rational(s.clone())), 1, ss.clone()),
            Some(_) => apply_operator_reverse(
                Parameters::Rational(s.clone()),
                Parameters::Identifier(ss.clone()),
                ram,
                mult,
            ),
        },
        (Parameters::Identifier(ss), Parameters::Rational(s)) => match ram {
            Some(_) => apply_operator(
                Parameters::Identifier(ss),
                Parameters::Rational(s),
                ram,
                mult,
            ),
            None => Parameters::Var(Box::from(Parameters::Rational(s.clone())), 1, ss.clone()),
        },
        (Parameters::Int(i), Parameters::Identifier(s)) => match ram {
            None => Parameters::Var(Box::from(Parameters::Int(i)), 1, s.clone()),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, mult),
        },
        (Parameters::Identifier(s), Parameters::InterpreterVector(vec)) => match ram {
            None => Parameters::InterpreterVector(vec.clone()),
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::InterpreterVector(vec.clone()),
                ram,
                mult,
            ),
        },
        (Parameters::InterpreterVector(vec), Parameters::Identifier(s)) => match ram {
            None => Parameters::InterpreterVector(vec.clone()),
            Some(_) => apply_operator_reverse(
                Parameters::InterpreterVector(vec.clone()),
                Parameters::Identifier(s),
                ram,
                mult,
            ),
        },
        (Parameters::Null, Parameters::Identifier(s)) => match ram {
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, mult),
            None => Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone()),
        },
        (Parameters::Identifier(s), Parameters::Null) => match ram {
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, mult),
            None => Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone()),
        },
        (Parameters::Identifier(s), Parameters::Float(i)) => match ram {
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, mult),
            None => Parameters::Var(Box::from(Parameters::Float(i)), 1, s.clone()),
        },
        (Parameters::Float(i), Parameters::Identifier(s)) => match ram {
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, mult),
            None => Parameters::Var(Box::from(Parameters::Float(i)), 1, s.clone()),
        },
        (Bool(b), Parameters::Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, mult),
            None => Parameters::Bool(b),
        },
        (Parameters::Identifier(s), Bool(b)) => match ram {
            Some(_) => apply_operator(Parameters::Identifier(s), Bool(b), ram, mult),
            None => Parameters::Bool(b),
        },
        _ => Parameters::Identifier(
            "@Those two values are incompatible with the * operator".to_string(),
        ),
    }
}
