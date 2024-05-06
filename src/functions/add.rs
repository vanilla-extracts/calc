use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;
use std::collections::HashMap;

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
                    Parameters::Var(Box::from(Parameters::Int(2)), 1, s.clone())
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
        (Parameters::Plus(s1, s2), Parameters::Plus(s3, s4)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), *s3.clone(), ram)),
                Box::from(add(*s2.clone(), *s4.clone(), ram)),
            );
            let second = Parameters::Plus(
                Box::from(add(*s1.clone(), *s4.clone(), ram)),
                Box::from(add(*s2.clone(), *s3.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));

            if s1 > s2 {
                second
            } else {
                first
            }
        }
        (Parameters::Plus(s1, s2), Parameters::Identifier(s3)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Identifier(s3.clone()), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Identifier(s3.clone()), ram)),
            );
            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Identifier(s3), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Identifier(s3.clone()), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Identifier(s3.clone()), ram)),
            );
            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Int(i), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Int(i), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Int(i), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Plus(s1, s2), Parameters::Int(i)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Int(i), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Int(i), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Plus(s1, s2), Parameters::Float(f)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Float(f), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Float(f), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Float(f), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Float(f), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Float(f), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Plus(s1, s2), Parameters::Rational(r)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Rational(r.clone()), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Rational(r.clone()), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Rational(r), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Plus(
                Box::from(add(*s1.clone(), Parameters::Rational(r.clone()), ram)),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Parameters::Rational(r.clone()), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Plus(s1, s2), Parameters::Var(x, y, z)) => {
            let first = Parameters::Plus(
                Box::from(add(
                    *s1.clone(),
                    Parameters::Var(x.clone(), y, z.clone()),
                    ram,
                )),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(
                    *s2.clone(),
                    Parameters::Var(x.clone(), y, z.clone()),
                    ram,
                )),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Var(x, y, z), Parameters::Plus(s1, s2)) => {
            let first = Parameters::Plus(
                Box::from(add(
                    Parameters::Var(x.clone(), y, z.clone()),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            let second = Parameters::Plus(
                s1.clone(),
                Box::from(add(
                    Parameters::Var(x.clone(), y, z.clone()),
                    *s2.clone(),
                    ram,
                )),
            );

            let (s1, s2) = (size(&first), size(&second));

            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Parameters::Plus(s1, s2), Parameters::Null) => Parameters::Plus(s1.clone(), s2.clone()),

        (Parameters::Null, Parameters::Plus(s1, s2)) => Parameters::Plus(s1.clone(), s2.clone()),

        (Parameters::Mul(s1, s2), Parameters::Mul(s3, s4)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Mul(s3.clone(), s4.clone())),
        ),

        (Parameters::Mul(s1, s2), Parameters::Identifier(s)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
        ),

        (Parameters::Identifier(s), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
        ),

        (Parameters::Mul(s1, s2), Parameters::Int(i)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Int(i)),
        ),

        (Parameters::Int(i), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(Parameters::Int(i)),
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
        ),

        (Parameters::Mul(s1, s2), Parameters::Float(f)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Float(f)),
        ),

        (Parameters::Float(f), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(Parameters::Float(f)),
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
        ),

        (Parameters::Mul(s1, s2), Parameters::Rational(r)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Rational(r.clone())),
        ),

        (Parameters::Rational(r), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(Parameters::Rational(r.clone())),
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
        ),

        (Parameters::Var(x, y, z), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(Parameters::Var(x.clone(), y, z.clone())),
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
        ),

        (Parameters::Mul(s1, s2), Parameters::Var(x, y, z)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Var(x.clone(), y, z.clone())),
        ),

        (Parameters::Mul(s1, s2), Parameters::Plus(s3, s4)) => Parameters::Plus(
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
            Box::from(Parameters::Plus(s3.clone(), s4.clone())),
        ),

        (Parameters::Plus(s3, s4), Parameters::Mul(s1, s2)) => Parameters::Plus(
            Box::from(Parameters::Plus(s3.clone(), s4.clone())),
            Box::from(Parameters::Mul(s1.clone(), s2.clone())),
        ),

        (Parameters::Null, Parameters::Mul(s1, s2)) => Parameters::Mul(s1.clone(), s2.clone()),

        (Parameters::Mul(s1, s2), Parameters::Null) => Parameters::Mul(s1.clone(), s2.clone()),

        (Parameters::Var(x, y, z), Parameters::Var(x1, y1, z1)) => {
            if z == z1 && y == y1 {
                Parameters::Var(Box::from(add(*x.clone(), *x1.clone(), ram)), y, z)
            } else {
                Parameters::Plus(
                    Box::from(Parameters::Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Parameters::Var(x1.clone(), y1.clone(), z1.clone())),
                )
            }
        }

        (Parameters::Var(x, y, z), Parameters::Identifier(s)) => {
            if z == s && y == 1 {
                Parameters::Var(Box::from(add(*x.clone(), Parameters::Int(1), ram)), y, z)
            } else {
                Parameters::Plus(
                    Box::from(Parameters::Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
                )
            }
        }

        (Parameters::Identifier(s), Parameters::Var(x, y, z)) => {
            if z == s && y == 1 {
                Parameters::Var(Box::from(add(*x.clone(), Parameters::Int(1), ram)), y, z)
            } else {
                Parameters::Plus(
                    Box::from(Parameters::Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Parameters::Var(Box::from(Parameters::Int(1)), 1, s.clone())),
                )
            }
        }

        (Parameters::Int(i), Parameters::Var(x, y, z)) => Parameters::Plus(
            Box::from(Parameters::Int(i)),
            Box::from(Parameters::Var(x, y, z)),
        ),

        (Parameters::Var(x, y, z), Parameters::Int(i)) => Parameters::Plus(
            Box::from(Parameters::Var(x, y, z)),
            Box::from(Parameters::Int(i)),
        ),

        (Parameters::Float(f), Parameters::Var(x, y, z)) => Parameters::Plus(
            Box::from(Parameters::Float(f)),
            Box::from(Parameters::Var(x, y, z)),
        ),

        (Parameters::Var(x, y, z), Parameters::Float(f)) => Parameters::Plus(
            Box::from(Parameters::Var(x, y, z)),
            Box::from(Parameters::Float(f)),
        ),

        (Parameters::Rational(r), Parameters::Var(x, y, z)) => Parameters::Plus(
            Box::from(Parameters::Rational(r.clone())),
            Box::from(Parameters::Var(x, y, z)),
        ),

        (Parameters::Var(x, y, z), Parameters::Rational(r)) => Parameters::Plus(
            Box::from(Parameters::Var(x, y, z)),
            Box::from(Parameters::Rational(r.clone())),
        ),

        _ => Parameters::Identifier(
            "@Those two values are incompatible with the + operator".to_string(),
        ),
    }
}
