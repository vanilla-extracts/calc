use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;
use std::collections::HashMap;

use super::add::add;
use super::mult::mult;

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
        (Parameters::Plus(s1, s2), Parameters::Plus(s3, s4)) => {
            let first = add(
                divide(*s1.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                divide(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                ram,
            );
            first
        }
        (Parameters::Plus(s1, s2), Parameters::Identifier(s3)) => {
            let first = add(
                divide(
                    *s1.clone(),
                    Parameters::Var(Box::from(Parameters::Int(1)), 1, s3.clone()),
                    ram,
                ),
                divide(
                    *s2.clone(),
                    Parameters::Var(Box::from(Parameters::Int(1)), 1, s3.clone()),
                    ram,
                ),
                ram,
            );
            first
        }
        (Parameters::Identifier(s3), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Div(
                Box::from(Parameters::Var(Box::new(Parameters::Int(1)), 1, s3.clone())),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }
        (Parameters::Int(i), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Div(
                Box::from(Parameters::Int(i)),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }

        (Parameters::Plus(s1, s2), Parameters::Int(i)) => {
            let first = add(
                divide(*s1.clone(), Parameters::Int(i), ram),
                divide(*s2.clone(), Parameters::Int(i), ram),
                ram,
            );
            first
        }

        (Parameters::Float(f), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Div(
                Box::from(Parameters::Float(f)),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }

        (Parameters::Plus(s1, s2), Parameters::Float(f)) => {
            let first = add(
                divide(*s1.clone(), Parameters::Float(f), ram),
                divide(*s2.clone(), Parameters::Float(f), ram),
                ram,
            );
            first
        }
        (Parameters::Rational(r), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Div(
                Box::from(Parameters::Rational(r.clone())),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }

        (Parameters::Plus(s1, s2), Parameters::Rational(r)) => {
            let first = add(
                divide(*s1.clone(), Parameters::Rational(r.clone()), ram),
                divide(*s2.clone(), Parameters::Rational(r.clone()), ram),
                ram,
            );
            first
        }
        (Parameters::Var(x, y, z), Parameters::Plus(s1, s2)) => Parameters::Div(
            Box::from(Parameters::Var(x.clone(), y.clone(), z.clone())),
            Box::from(add(*s1.clone(), *s2.clone(), ram)),
        ),

        (Parameters::Plus(s1, s2), Parameters::Var(x, y, z)) => add(
            divide(*s1.clone(), Parameters::Var(x.clone(), y, z.clone()), ram),
            divide(*s2.clone(), Parameters::Var(x.clone(), y, z.clone()), ram),
            ram,
        ),

        (Parameters::Null, Parameters::Plus(s1, s2)) => add(*s1.clone(), *s2.clone(), ram),

        (Parameters::Plus(s1, s2), Parameters::Null) => add(*s1.clone(), *s2.clone(), ram),

        //x/yz = x/y * 1/z | 1/y * x/z
        (Parameters::Var(x, y, z), Parameters::Mul(s1, s2)) => {
            let first = mult(
                divide(Parameters::Var(x.clone(), y, z.clone()), *s1.clone(), ram),
                divide(Parameters::Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Parameters::Int(1), *s1.clone(), ram),
                divide(Parameters::Var(x.clone(), y, z.clone()), *s2.clone(), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        //xy/z = x/z * y | y/z * x
        (Parameters::Mul(s1, s2), Parameters::Var(x, y, z)) => {
            let first = mult(
                divide(*s1.clone(), Parameters::Var(x.clone(), y, z.clone()), ram),
                *s2.clone(),
                ram,
            );
            let second = mult(
                *s1.clone(),
                divide(*s2.clone(), Parameters::Var(x.clone(), y, z.clone()), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }
        //xy/ab = x/ab * y/1 | y/ab * x
        (Parameters::Mul(s1, s2), Parameters::Mul(s3, s4)) => {
            let first = mult(
                divide(*s1.clone(), mult(*s3.clone(), *s4.clone(), ram), ram),
                *s2.clone(),
                ram,
            );
            let second = mult(
                *s1.clone(),
                divide(*s2.clone(), mult(*s3.clone(), *s4.clone(), ram), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));
            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        //xy/a = x/a * y | y/a * x
        (Parameters::Mul(s1, s2), Parameters::Identifier(s)) => {
            let first = mult(
                divide(
                    *s1.clone(),
                    Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone()),
                    ram,
                ),
                *s2.clone(),
                ram,
            );
            let second = mult(
                *s1.clone(),
                divide(
                    *s2.clone(),
                    Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone()),
                    ram,
                ),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }
        (Parameters::Identifier(s), Parameters::Mul(s1, s2)) => {
            let first = mult(
                divide(
                    Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone()),
                    *s1.clone(),
                    ram,
                ),
                divide(Parameters::Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Parameters::Int(1), *s1.clone(), ram),
                divide(
                    Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone()),
                    *s2.clone(),
                    ram,
                ),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Parameters::Mul(s1, s2), Parameters::Int(i)) => Parameters::Mul(
            Box::from(mult(*s1.clone(), Parameters::Int(i), ram)),
            s2.clone(),
        ),

        (Parameters::Int(i), Parameters::Mul(s1, s2)) => Parameters::Mul(
            Box::from(mult(*s1.clone(), Parameters::Int(i), ram)),
            s2.clone(),
        ),

        (Parameters::Mul(s1, s2), Parameters::Float(f)) => Parameters::Mul(
            Box::from(mult(*s1.clone(), Parameters::Float(f), ram)),
            s2.clone(),
        ),

        (Parameters::Float(f), Parameters::Mul(s1, s2)) => Parameters::Mul(
            Box::from(mult(*s1.clone(), Parameters::Float(f), ram)),
            s2.clone(),
        ),

        (Parameters::Mul(s1, s2), Parameters::Rational(r)) => Parameters::Mul(
            Box::from(mult(*s1.clone(), Parameters::Rational(r.clone()), ram)),
            s2.clone(),
        ),

        (Parameters::Rational(r), Parameters::Mul(s1, s2)) => Parameters::Mul(
            Box::from(mult(*s1.clone(), Parameters::Rational(r.clone()), ram)),
            s2.clone(),
        ),
        //(x*y)*(a+b) = x*y*a+x*y*b
        (Parameters::Mul(s1, s2), Parameters::Plus(s3, s4)) => Parameters::Plus(
            Box::from(mult(mult(*s1.clone(), *s3.clone(), ram), *s2.clone(), ram)),
            Box::from(mult(mult(*s1.clone(), *s4.clone(), ram), *s2.clone(), ram)),
        ),

        (Parameters::Plus(s3, s4), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(mult(mult(*s1.clone(), *s3.clone(), ram), *s2.clone(), ram)),
            Box::from(mult(mult(*s1.clone(), *s4.clone(), ram), *s2.clone(), ram)),
        ),

        (Parameters::Null, Parameters::Mul(s1, s2)) => Parameters::Mul(s1.clone(), s2.clone()),

        (Parameters::Mul(s1, s2), Parameters::Null) => Parameters::Mul(s1.clone(), s2.clone()),

        (Parameters::Var(x, y, z), Parameters::Var(x1, y1, z1)) => {
            if z == z1 {
                Parameters::Var(Box::from(mult(*x.clone(), *x1.clone(), ram)), y + y1, z)
            } else {
                Parameters::Mul(
                    Box::from(Parameters::Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Parameters::Var(
                        Box::from(mult(Parameters::Int(1), *x1.clone(), ram)),
                        y1.clone(),
                        z1.clone(),
                    )),
                )
            }
        }

        //2x*x
        (Parameters::Var(x, y, z), Parameters::Identifier(s)) => {
            if z == s {
                Parameters::Var(Box::from(x.clone()), y + 1, z)
            } else {
                Parameters::Mul(
                    Box::from(Parameters::Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
                )
            }
        }

        (Parameters::Identifier(s), Parameters::Var(x, y, z)) => {
            if z == s {
                Parameters::Var(Box::from(x.clone()), y + 1, z)
            } else {
                Parameters::Mul(
                    Box::from(Parameters::Var(
                        Box::from(mult(Parameters::Int(1), *x.clone(), ram)),
                        y.clone(),
                        z.clone(),
                    )),
                    Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
                )
            }
        }

        (Parameters::Int(i), Parameters::Var(x, y, z)) => Parameters::Var(
            Box::from(mult(*x.clone(), Parameters::Int(i), ram)),
            y,
            z.clone(),
        ),

        (Parameters::Var(x, y, z), Parameters::Int(i)) => Parameters::Var(
            Box::from(mult(*x.clone(), Parameters::Int(i), ram)),
            y,
            z.clone(),
        ),

        (Parameters::Float(f), Parameters::Var(x, y, z)) => Parameters::Var(
            Box::from(mult(Parameters::Float(f), *x.clone(), ram)),
            y,
            z.clone(),
        ),

        (Parameters::Var(x, y, z), Parameters::Float(f)) => Parameters::Var(
            Box::from(mult(*x.clone(), Parameters::Float(f), ram)),
            y,
            z.clone(),
        ),

        (Parameters::Rational(r), Parameters::Var(x, y, z)) => Parameters::Var(
            Box::from(mult(Parameters::Rational(r.clone()), *x.clone(), ram)),
            y,
            z.clone(),
        ),

        (Parameters::Var(x, y, z), Parameters::Rational(r)) => Parameters::Var(
            Box::from(mult(*x.clone(), Parameters::Rational(r), ram)),
            y,
            z.clone(),
        ),
        _ => Parameters::Identifier(
            "@Those two values are incompatible with the / operator".to_string(),
        ),
    }
}
