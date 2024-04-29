use std::collections::HashMap;

use crate::exact_math::rationals::Rationals;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::Bool;
use crate::utils::matrix_utils::mult_matrix;

pub fn apply_operator(
    value: Parameters,
    value2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
    f: fn(Parameters, Parameters, Option<&HashMap<String, Parameters>>) -> Parameters,
) -> Parameters {
    let s = match value {
        Parameters::Identifier(ref s) => s,
        _ => "",
    };
    if s == "" {
        return Parameters::Null;
    }
    match ram {
        None => f(value.clone(), value2.clone(), None),
        Some(i_ram) => {
            let values = i_ram.get(s);
            match values {
                None => f(value.clone(), value2.clone(), None),
                Some(val) => f(val.clone(), value2.clone(), ram),
            }
        }
    }
}

pub fn apply_operator_reverse(
    value: Parameters,
    value2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
    f: fn(Parameters, Parameters, Option<&HashMap<String, Parameters>>) -> Parameters,
) -> Parameters {
    let s = match value2 {
        Parameters::Identifier(s) => s,
        _ => "".to_string(),
    };
    if s == "".to_string() {
        return Parameters::Null;
    }
    match ram {
        None => value,
        Some(i_ram) => {
            let val3 = i_ram.get(&s);
            match val3 {
                None => value,
                Some(val) => f(value.clone(), val.clone(), ram),
            }
        }
    }
}

pub fn add(i: Parameters, i2: Parameters, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(v)) => Parameters::Int(v),
        (Parameters::Null, Parameters::Float(f)) => Parameters::Float(f),
        (Parameters::Null, Parameters::InterpreterVector(vec)) => {
            Parameters::InterpreterVector(vec.clone())
        }
        (Parameters::InterpreterVector(vec), Parameters::Null) => {
            Parameters::InterpreterVector(vec.clone())
        }
        (Parameters::Int(v), Parameters::Null) => Parameters::Int(v),
        (Parameters::Float(f), Parameters::Null) => Parameters::Float(f),
        (Parameters::Rational(s), Parameters::Null) => Parameters::Rational(s.clone()),
        (Parameters::Null, Parameters::Rational(s)) => Parameters::Rational(s.clone()),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Parameters::Rational(s + s2),
        (Parameters::Rational(s), Parameters::Int(i)) => {
            Parameters::Rational(s + Rationals::new(1, i))
        }
        (Parameters::Int(i), Parameters::Rational(s)) => {
            Parameters::Rational(s + Rationals::new(1, i))
        }
        (Parameters::Rational(s), Parameters::Float(f)) => Parameters::Float(s.approx() + f),
        (Parameters::Float(f), Parameters::Rational(s)) => Parameters::Float(f + s.approx()),
        (Parameters::Int(v), Parameters::Int(v2)) => Parameters::Int(v + v2),
        (Parameters::Int(v), Parameters::Float(f)) => Parameters::Float((v as f64) + f),
        (Parameters::Float(v), Parameters::Float(f)) => Parameters::Float(v + f),
        (Parameters::Float(v), Parameters::Int(i1)) => Parameters::Float(v + (i1 as f64)),
        (Parameters::InterpreterVector(vec), Parameters::InterpreterVector(vec2)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .zip(vec2.into_iter())
                .map(|(x, y)| add(x.clone(), y.clone(), ram))
                .for_each(|s| res.push(s));
            Parameters::InterpreterVector(Box::from(res))
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
                if s != s2 {
                    Parameters::Plus(
                        Box::from(Parameters::Identifier(s.clone())),
                        Box::from(Parameters::Identifier(s2.clone())),
                    )
                } else {
                    Parameters::Mul(
                        Box::from(Parameters::Int(2)),
                        Box::from(Parameters::Identifier(s.clone())),
                    )
                }
            }
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::Identifier(s2),
                ram,
                add,
            ),
        },
        (Parameters::Identifier(s), Parameters::Int(i)) => match ram {
            None => Parameters::Plus(
                Box::from(Parameters::Identifier(s.clone())),
                Box::from(Parameters::Int(i)),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, add),
        },
        (Parameters::Null, Parameters::Identifier(s)) => match ram {
            None => Parameters::Identifier(s.clone()),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, add),
        },
        (Parameters::Rational(s), Parameters::Identifier(ss)) => match ram {
            None => Parameters::Plus(
                Box::from(Parameters::Rational(s.clone())),
                Box::from(Parameters::Identifier(ss.clone())),
            ),
            Some(_) => apply_operator_reverse(
                Parameters::Rational(s.clone()),
                Parameters::Identifier(ss.clone()),
                ram,
                add,
            ),
        },
        (Parameters::Identifier(ss), Parameters::Rational(s)) => match ram {
            None => Parameters::Plus(
                Box::from(Parameters::Identifier(ss.clone())),
                Box::from(Parameters::Rational(s.clone())),
            ),
            Some(_) => apply_operator(
                Parameters::Identifier(ss),
                Parameters::Rational(s),
                ram,
                add,
            ),
        },
        (Parameters::Identifier(s), Parameters::Null) => match ram {
            None => Parameters::Identifier(s.clone()),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Null, ram, add),
        },
        (Parameters::Int(i), Parameters::Identifier(s)) => match ram {
            None => Parameters::Plus(
                Box::from(Parameters::Identifier(s)),
                Box::from(Parameters::Int(i.clone())),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, add),
        },
        (Parameters::Identifier(s), Parameters::Float(i)) => match ram {
            None => Parameters::Plus(
                Box::from(Parameters::Identifier(s.clone())),
                Box::from(Parameters::Float(i)),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, add),
        },
        (Parameters::Float(i), Parameters::Identifier(s)) => match ram {
            None => Parameters::Plus(
                Box::from(Parameters::Identifier(s.clone())),
                Box::from(Parameters::Float(i)),
            ),
            Some(_) => apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, add),
        },
        (Parameters::Identifier(s), Parameters::InterpreterVector(vec)) => match ram {
            None => Parameters::Null,
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::InterpreterVector(vec.clone()),
                ram,
                add,
            ),
        },
        (Parameters::InterpreterVector(vec), Parameters::Identifier(s)) => match ram {
            None => Parameters::Null,
            Some(_) => apply_operator(
                Parameters::Identifier(s),
                Parameters::InterpreterVector(vec.clone()),
                ram,
                add,
            ),
        },
        (Bool(b), Parameters::Identifier(s)) => match ram {
            None => Parameters::Null,
            Some(_) => apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, add),
        },
        (Parameters::Identifier(s), Bool(b)) => match ram {
            None => Parameters::Null,
            Some(_) => apply_operator(Parameters::Identifier(s), Bool(b), ram, add),
        },
        (Parameters::Plus(s1, s2), Parameters::Identifier(s3)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Identifier(s3.clone()), ram)),
                s2.clone(),
            );
            let _second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Identifier(s3.clone()), ram)),
            );
            first
        }
        _ => Parameters::Identifier(
            "@Those two values are incompatible with the + operator".to_string(),
        ),
    }
}

pub fn minus(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(v)) => Parameters::Int(-v),
        (Parameters::Null, Parameters::Float(f)) => Parameters::Float(-f),
        (Parameters::Int(v), Parameters::Null) => Parameters::Int(-v),
        (Parameters::Float(f), Parameters::Null) => Parameters::Float(-f),
        (Parameters::Int(v), Parameters::Int(v2)) => Parameters::Int(v - v2),

        (Parameters::Rational(s), Parameters::Null) => {
            Parameters::Rational(Rationals::new(1, 0) - s)
        }

        (Parameters::Null, Parameters::Rational(s)) => {
            Parameters::Rational(Rationals::new(1, 0) - s)
        }
        (Parameters::Rational(s), Parameters::Rational(s2)) => Parameters::Rational(s - s2),
        (Parameters::Rational(s), Parameters::Int(i)) => {
            Parameters::Rational(s - Rationals::new(1, i))
        }
        (Parameters::Int(i), Parameters::Rational(s)) => {
            Parameters::Rational(Rationals::new(1, i) - s)
        }
        (Parameters::Rational(s), Parameters::Float(f)) => Parameters::Float(s.approx() - f),
        (Parameters::Float(f), Parameters::Rational(s)) => Parameters::Float(f - s.approx()),
        (Parameters::InterpreterVector(vec), Parameters::Null) => {
            let mut res = Vec::new();
            vec.into_iter()
                .map(|x| minus(Parameters::Null, x.clone(), ram))
                .for_each(|z| res.push(z));
            Parameters::InterpreterVector(Box::from(res))
        }

        (Parameters::Null, Parameters::InterpreterVector(vec)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .map(|x| minus(Parameters::Null, x.clone(), ram))
                .for_each(|z| res.push(z));
            Parameters::InterpreterVector(Box::from(res))
        }

        (Parameters::InterpreterVector(vec), Parameters::InterpreterVector(vec2)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .zip(vec2.into_iter())
                .map(|(x, y)| minus(x.clone(), y.clone(), ram))
                .for_each(|z| res.push(z));
            Parameters::InterpreterVector(Box::from(res))
        }
        (Parameters::Int(v), Parameters::Float(f)) => Parameters::Float((v as f64) - f),
        (Parameters::Float(v), Parameters::Float(f)) => Parameters::Float(v - f),
        (Parameters::Float(v), Parameters::Int(i1)) => Parameters::Float(v - (i1 as f64)),

        (Bool(_), Parameters::Int(i)) => Parameters::Int(i),
        (Bool(_), Parameters::Float(i)) => Parameters::Float(i),
        (Parameters::Int(i), Bool(_)) => Parameters::Int(i),
        (Parameters::Float(i), Bool(_)) => Parameters::Float(i),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            minus,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, minus)
        }
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, minus)
        }

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            minus,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            minus,
        ),
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, minus)
        }
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            let v = apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, minus);
            match v {
                Parameters::Int(i) => Parameters::Int(-i),
                _ => Parameters::Null,
            }
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, minus)
        }
        (Parameters::Float(i), Parameters::Identifier(s)) => {
            let v = apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, minus);
            match v {
                Parameters::Float(i) => Parameters::Float(-i),
                _ => Parameters::Null,
            }
        }

        (Parameters::InterpreterVector(vec), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::InterpreterVector(vec.clone()),
            Parameters::Identifier(s),
            ram,
            minus,
        ),
        (Parameters::Identifier(s), Parameters::InterpreterVector(vec)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::InterpreterVector(vec.clone()),
            ram,
            minus,
        ),
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, minus)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, minus)
        }
        _ => Parameters::Identifier(
            "Those two values are incompatible with the - operator".to_string(),
        ),
    }
}

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
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            mult,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, mult)
        }

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            mult,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            mult,
        ),
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, mult)
        }
        (Parameters::Identifier(s), Parameters::InterpreterVector(vec)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::InterpreterVector(vec.clone()),
            ram,
            mult,
        ),
        (Parameters::InterpreterVector(vec), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::InterpreterVector(vec.clone()),
            Parameters::Identifier(s),
            ram,
            mult,
        ),
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, mult)
        }
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, mult)
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, mult)
        }
        (Parameters::Float(i), Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, mult)
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, mult)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, mult)
        }
        _ => Parameters::Identifier(
            "@Those two values are incompatible with the * operator".to_string(),
        ),
    }
}

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
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            divide,
        ),

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            divide,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            divide,
        ),

        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, divide)
        }
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            let v = apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, divide);
            match v {
                Parameters::Float(i) => Parameters::Float(1.0 / i),
                _ => Parameters::Null,
            }
        }
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, divide)
        }
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, divide)
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, divide)
        }
        (Parameters::Float(i), Parameters::Identifier(s)) => {
            let v = apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, divide);
            match v {
                Parameters::Float(i) => Parameters::Float(1.0 / i),
                _ => Parameters::Null,
            }
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, divide)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, divide)
        }
        _ => Parameters::Identifier(
            "@Those two values are incompatible with the / operator".to_string(),
        ),
    }
}

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
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            expo,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, expo)
        }
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Int(i), Parameters::Identifier(s), ram, expo)
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, expo)
        }

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            expo,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            expo,
        ),
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, expo)
        }
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, expo)
        }
        (Parameters::Float(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Float(i), Parameters::Identifier(s), ram, expo)
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, expo)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, expo)
        }

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the ^ operator".to_string(),
        ),
    }
}

pub fn assign(s: Parameters, s2: Parameters) -> (String, Parameters) {
    match s {
        Parameters::Identifier(s) => (s, s2),
        _ => ("".to_string(), s2),
    }
}

pub fn greater(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(_)) => Bool(true),
        (Parameters::Null, Parameters::Float(_)) => Bool(true),
        (Parameters::Int(_), Parameters::Null) => Bool(true),
        (Parameters::Float(_), Parameters::Null) => Bool(true),
        (Parameters::Int(v), Parameters::Int(v2)) => Bool(v > v2),
        (Parameters::Int(v), Parameters::Float(f)) => Bool((v as f64) > f),
        (Parameters::Float(v), Parameters::Float(f)) => Bool(v > f),
        (Parameters::Float(v), Parameters::Int(i1)) => Bool(v > (i1 as f64)),
        (Parameters::Rational(_), Parameters::Null) => Bool(true),
        (Parameters::Null, Parameters::Rational(_)) => Bool(true),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Bool(s > s2),
        (Parameters::Rational(s), Parameters::Int(i)) => Bool(s > Rationals::new(1, i)),
        (Parameters::Int(i), Parameters::Rational(s)) => Bool(Rationals::new(1, i) > s),
        (Parameters::Rational(s), Parameters::Float(f)) => Bool(s.approx() > f),
        (Parameters::Float(f), Parameters::Rational(s)) => Bool(f > s.approx()),
        (Bool(b), Parameters::Int(_)) => Bool(b),
        (Bool(b), Parameters::Float(_)) => Bool(b),
        (Parameters::Int(_), Bool(b)) => Bool(b),
        (Parameters::Float(_), Bool(b)) => Bool(b),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            greater,
        ),

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            greater,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            greater,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, greater)
        }
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, greater)
        }
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, greater)
        }
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Int(i), Parameters::Identifier(s), ram, greater)
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Float(i),
            ram,
            greater,
        ),
        (Parameters::Float(i), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::Float(i),
            Parameters::Identifier(s),
            ram,
            greater,
        ),
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, greater)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, greater)
        }

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the > operator".to_string(),
        ),
    }
}

pub fn lesser(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(_)) => Bool(false),
        (Parameters::Null, Parameters::Float(_)) => Bool(false),
        (Parameters::Int(_), Parameters::Null) => Bool(false),
        (Parameters::Float(_), Parameters::Null) => Bool(false),
        (Parameters::Int(v), Parameters::Int(v2)) => Bool(v < v2),
        (Parameters::Int(v), Parameters::Float(f)) => Bool((v as f64) < f),
        (Parameters::Float(v), Parameters::Float(f)) => Bool(v < f),
        (Parameters::Float(v), Parameters::Int(i1)) => Bool(v < (i1 as f64)),
        (Parameters::Rational(_), Parameters::Null) => Bool(true),
        (Parameters::Null, Parameters::Rational(_)) => Bool(true),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Bool(s < s2),
        (Parameters::Rational(s), Parameters::Int(i)) => Bool(s < Rationals::new(1, i)),
        (Parameters::Int(i), Parameters::Rational(s)) => Bool(Rationals::new(1, i) < s),
        (Parameters::Rational(s), Parameters::Float(f)) => Bool(s.approx() < f),
        (Parameters::Float(f), Parameters::Rational(s)) => Bool(f < s.approx()),
        (Bool(b), Parameters::Int(_)) => Bool(b),
        (Bool(b), Parameters::Float(_)) => Bool(b),
        (Parameters::Int(_), Bool(b)) => Bool(b),
        (Parameters::Float(_), Bool(b)) => Bool(b),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            lesser,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, lesser)
        }
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, lesser)
        }
        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            lesser,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            lesser,
        ),
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, lesser)
        }
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Int(i), Parameters::Identifier(s), ram, lesser)
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, lesser)
        }
        (Parameters::Float(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Float(i), Parameters::Identifier(s), ram, lesser)
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, lesser)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, lesser)
        }

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the < operator".to_string(),
        ),
    }
}

pub fn greater_or_equal(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(_)) => Bool(true),
        (Parameters::Null, Parameters::Float(_)) => Bool(true),
        (Parameters::Int(_), Parameters::Null) => Bool(true),
        (Parameters::Float(_), Parameters::Null) => Bool(true),
        (Parameters::Int(v), Parameters::Int(v2)) => Bool(v >= v2),
        (Parameters::Int(v), Parameters::Float(f)) => Bool((v as f64) >= f),
        (Parameters::Float(v), Parameters::Float(f)) => Bool(v >= f),
        (Parameters::Float(v), Parameters::Int(i1)) => Bool(v >= (i1 as f64)),
        (Bool(b), Parameters::Int(_)) => Bool(b),
        (Bool(b), Parameters::Float(_)) => Bool(b),
        (Parameters::Int(_), Bool(b)) => Bool(b),
        (Parameters::Float(_), Bool(b)) => Bool(b),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b == b2),

        (Parameters::Rational(_), Parameters::Null) => Bool(true),
        (Parameters::Null, Parameters::Rational(_)) => Bool(true),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Bool(s >= s2),
        (Parameters::Rational(s), Parameters::Int(i)) => Bool(s >= Rationals::new(1, i)),
        (Parameters::Int(i), Parameters::Rational(s)) => Bool(Rationals::new(1, i) >= s),
        (Parameters::Rational(s), Parameters::Float(f)) => Bool(s.approx() >= f),
        (Parameters::Float(f), Parameters::Rational(s)) => Bool(f >= s.approx()),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            greater_or_equal,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Int(i),
            ram,
            greater_or_equal,
        ),
        (Parameters::Null, Parameters::Identifier(s)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Null,
            ram,
            greater_or_equal,
        ),
        (Parameters::Identifier(s), Parameters::Null) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Null,
            ram,
            greater_or_equal,
        ),

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            greater_or_equal,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            greater_or_equal,
        ),
        (Parameters::Int(i), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::Int(i),
            Parameters::Identifier(s),
            ram,
            greater_or_equal,
        ),
        (Parameters::Identifier(s), Parameters::Float(i)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Float(i),
            ram,
            greater_or_equal,
        ),
        (Parameters::Float(i), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::Float(i),
            Parameters::Identifier(s),
            ram,
            greater_or_equal,
        ),
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, greater_or_equal)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, greater_or_equal)
        }

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the >= operator".to_string(),
        ),
    }
}

pub fn lesser_or_equal(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(_)) => Bool(false),
        (Parameters::Null, Parameters::Float(_)) => Bool(false),
        (Parameters::Int(_), Parameters::Null) => Bool(false),
        (Parameters::Float(_), Parameters::Null) => Bool(false),
        (Parameters::Int(v), Parameters::Int(v2)) => Bool(v <= v2),
        (Parameters::Int(v), Parameters::Float(f)) => Bool((v as f64) <= f),
        (Parameters::Float(v), Parameters::Float(f)) => Bool(v <= f),
        (Parameters::Float(v), Parameters::Int(i1)) => Bool(v <= (i1 as f64)),
        (Bool(b), Parameters::Int(_)) => Bool(b),
        (Bool(b), Parameters::Float(_)) => Bool(b),
        (Parameters::Int(_), Bool(b)) => Bool(b),
        (Parameters::Float(_), Bool(b)) => Bool(b),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b == b2),

        (Parameters::Rational(_), Parameters::Null) => Bool(true),
        (Parameters::Null, Parameters::Rational(_)) => Bool(true),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Bool(s <= s2),
        (Parameters::Rational(s), Parameters::Int(i)) => Bool(s <= Rationals::new(1, i)),
        (Parameters::Int(i), Parameters::Rational(s)) => Bool(Rationals::new(1, i) <= s),
        (Parameters::Rational(s), Parameters::Float(f)) => Bool(s.approx() <= f),
        (Parameters::Float(f), Parameters::Rational(s)) => Bool(f <= s.approx()),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            lesser_or_equal,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Int(i),
            ram,
            lesser_or_equal,
        ),

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            lesser_or_equal,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            lesser_or_equal,
        ),
        (Parameters::Null, Parameters::Identifier(s)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Null,
            ram,
            lesser_or_equal,
        ),
        (Parameters::Identifier(s), Parameters::Null) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Null,
            ram,
            lesser_or_equal,
        ),
        (Parameters::Int(i), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::Int(i),
            Parameters::Identifier(s),
            ram,
            lesser_or_equal,
        ),
        (Parameters::Identifier(s), Parameters::Float(i)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Float(i),
            ram,
            lesser_or_equal,
        ),
        (Parameters::Float(i), Parameters::Identifier(s)) => apply_operator_reverse(
            Parameters::Float(i),
            Parameters::Identifier(s),
            ram,
            lesser_or_equal,
        ),
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, lesser_or_equal)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, lesser_or_equal)
        }

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the <= operator".to_string(),
        ),
    }
}

pub fn equal(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Parameters::Null, Parameters::Int(_)) => Bool(true),
        (Parameters::Null, Parameters::Float(_)) => Bool(true),
        (Parameters::Int(_), Parameters::Null) => Bool(true),
        (Parameters::Float(_), Parameters::Null) => Bool(true),
        (Parameters::Int(v), Parameters::Int(v2)) => Bool(v == v2),
        (Parameters::Int(v), Parameters::Float(f)) => Bool((v as f64) == f),
        (Parameters::Float(v), Parameters::Float(f)) => Bool(v == f),
        (Parameters::Float(v), Parameters::Int(i1)) => Bool(v == (i1 as f64)),
        (Bool(_), Parameters::Int(_)) => Bool(false),
        (Bool(_), Parameters::Float(_)) => Bool(false),
        (Parameters::Int(_), Bool(_)) => Bool(false),
        (Parameters::Float(_), Bool(_)) => Bool(false),
        (Bool(_), Parameters::Null) => Bool(false),
        (Parameters::Null, Bool(_)) => Bool(false),
        (Bool(b), Bool(b2)) => Bool(b == b2),

        (Parameters::Rational(_), Parameters::Null) => Bool(true),
        (Parameters::Null, Parameters::Rational(_)) => Bool(true),
        (Parameters::Rational(s), Parameters::Rational(s2)) => Bool(s == s2),
        (Parameters::Rational(s), Parameters::Int(i)) => Bool(s == Rationals::new(1, i)),
        (Parameters::Int(i), Parameters::Rational(s)) => Bool(Rationals::new(1, i) == s),
        (Parameters::Rational(s), Parameters::Float(f)) => Bool(s.approx() == f),
        (Parameters::Float(f), Parameters::Rational(s)) => Bool(f == s.approx()),
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            equal,
        ),
        (Parameters::Identifier(s), Parameters::Int(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Int(i), ram, equal)
        }
        (Parameters::Null, Parameters::Identifier(s)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, equal)
        }
        (Parameters::Identifier(s), Parameters::Null) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, equal)
        }
        (Parameters::Int(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Int(i), Parameters::Identifier(s), ram, equal)
        }
        (Parameters::Identifier(s), Parameters::Float(i)) => {
            apply_operator(Parameters::Identifier(s), Parameters::Float(i), ram, equal)
        }
        (Parameters::Float(i), Parameters::Identifier(s)) => {
            apply_operator_reverse(Parameters::Float(i), Parameters::Identifier(s), ram, equal)
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, equal)
        }
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, equal)
        }

        (Parameters::Rational(s), Parameters::Identifier(ss)) => apply_operator_reverse(
            Parameters::Rational(s.clone()),
            Parameters::Identifier(ss.clone()),
            ram,
            equal,
        ),
        (Parameters::Identifier(ss), Parameters::Rational(s)) => apply_operator(
            Parameters::Identifier(ss),
            Parameters::Rational(s),
            ram,
            equal,
        ),

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the == operator".to_string(),
        ),
    }
}

pub fn not(
    i: Parameters,
    _i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match i {
        Bool(b) => Bool(!b),
        Parameters::Identifier(s) => {
            apply_operator(Parameters::Identifier(s), Parameters::Null, ram, not)
        }
        _ => Bool(false),
    }
}

pub fn and(i: Parameters, i2: Parameters, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
    match (i, i2) {
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, and)
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, and)
        }
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            and,
        ),
        _ => Bool(false),
    }
}

pub fn or(i: Parameters, i2: Parameters, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
    match (i, i2) {
        (Bool(b), Bool(b2)) => Bool(b || b2),
        (Bool(b), Parameters::Null) => Bool(b),
        (Parameters::Null, Bool(b)) => Bool(b),
        (Parameters::Identifier(s), Bool(b)) => {
            apply_operator(Parameters::Identifier(s), Bool(b), ram, or)
        }
        (Bool(b), Parameters::Identifier(s)) => {
            apply_operator_reverse(Bool(b), Parameters::Identifier(s), ram, or)
        }
        (Parameters::Identifier(s), Parameters::Identifier(s2)) => apply_operator(
            Parameters::Identifier(s),
            Parameters::Identifier(s2),
            ram,
            or,
        ),
        _ => Bool(false),
    }
}

#[cfg(test)]
mod test {
    use crate::interpreting::function::{add, divide, minus, mult};
    use crate::parsing::ast::Parameters;

    #[test]
    pub fn test_add_null() {
        let expected = Parameters::Int(1);
        let result = add(Parameters::Int(1), Parameters::Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_simple() {
        let expected = Parameters::Int(2);
        let result = add(Parameters::Int(1), Parameters::Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_float() {
        let expected = Parameters::Float(2.1);
        let result = add(Parameters::Float(0.1), Parameters::Float(2.0), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_int_float() {
        let expected = Parameters::Float(2.1);
        let result = add(Parameters::Int(2), Parameters::Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_float_int() {
        let expected = Parameters::Float(2.1);
        let result = add(Parameters::Float(0.1), Parameters::Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_null() {
        let expected = Parameters::Int(-1);
        let result = minus(Parameters::Int(1), Parameters::Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_null_rev() {
        let expected = Parameters::Int(-1);
        let result = minus(Parameters::Null, Parameters::Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_simple() {
        let expected = Parameters::Int(0);
        let result = minus(Parameters::Int(1), Parameters::Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_float() {
        let expected = Parameters::Float(1.9);
        let result = minus(Parameters::Float(2.0), Parameters::Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_int_float() {
        let expected = Parameters::Float(1.9);
        let result = minus(Parameters::Int(2), Parameters::Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_float_int() {
        let expected = Parameters::Float(-1.9);
        let result = minus(Parameters::Float(0.1), Parameters::Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_null() {
        let expected = Parameters::Int(1);
        let result = mult(Parameters::Int(1), Parameters::Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_simple() {
        let expected = Parameters::Int(2);
        let result = mult(Parameters::Int(1), Parameters::Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_float() {
        let expected = Parameters::Float(0.2);
        let result = mult(Parameters::Float(0.1), Parameters::Float(2.0), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_int_float() {
        let expected = Parameters::Float(0.2);
        let result = mult(Parameters::Int(2), Parameters::Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_float_int() {
        let expected = Parameters::Float(0.2);
        let result = mult(Parameters::Float(0.1), Parameters::Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_null() {
        let expected = Parameters::Int(1);
        let result = divide(Parameters::Int(1), Parameters::Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_simple() {
        let expected =
            Parameters::Rational(crate::exact_math::rationals::Rationals { under: 1, over: 1 });
        let result = divide(Parameters::Int(1), Parameters::Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_float() {
        let expected = Parameters::Float(0.05);
        let result = divide(Parameters::Float(0.1), Parameters::Float(2.0), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_int_float() {
        let expected = Parameters::Float(20.0);
        let result = divide(Parameters::Int(2), Parameters::Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_float_int() {
        let expected = Parameters::Float(0.05);
        let result = divide(Parameters::Float(0.1), Parameters::Int(2), None);
        assert_eq!(result, expected);
    }
}
