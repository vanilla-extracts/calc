use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;
use std::collections::HashMap;

use super::add::add;
use super::divide::divide;
use super::mult::mult;

pub fn minus(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(v)) => Int(-v),
        (Null, Float(f)) => Float(-f),
        (Int(v), Null) => Int(-v),
        (Float(f), Null) => Float(-f),
        (Int(v), Int(v2)) => Int(v - v2),

        (Rational(s), Null) => Rational(Rationals::new(1, 0) - s),

        (Null, Rational(s)) => Rational(s.opposite()),
        (Rational(s), Rational(s2)) => Rational(s - s2),
        (Rational(s), Int(i)) => Rational(s - Rationals::new(1, i)),
        (Int(i), Rational(s)) => Rational(Rationals::new(1, i) - s),
        (Rational(s), Float(f)) => Float(s.approx() - f),
        (Float(f), Rational(s)) => Float(f - s.approx()),
        (InterpreterVector(vec), Null) => {
            let mut res = Vec::new();
            vec.into_iter()
                .map(|x| minus(Null, x.clone(), ram))
                .for_each(|z| res.push(z));
            InterpreterVector(Box::from(res))
        }

        (Null, InterpreterVector(vec)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .map(|x| minus(Null, x.clone(), ram))
                .for_each(|z| res.push(z));
            InterpreterVector(Box::from(res))
        }

        (InterpreterVector(vec), InterpreterVector(vec2)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .zip(vec2.into_iter())
                .map(|(x, y)| minus(x.clone(), y.clone(), ram))
                .for_each(|z| res.push(z));
            InterpreterVector(Box::from(res))
        }
        (Int(v), Float(f)) => Float((v as f64) - f),
        (Float(v), Float(f)) => Float(v - f),
        (Float(v), Int(i1)) => Float(v - (i1 as f64)),

        (Bool(_), Int(i)) => Int(i),
        (Bool(_), Float(i)) => Float(i),
        (Int(i), Bool(_)) => Int(i),
        (Float(i), Bool(_)) => Float(i),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Identifier(s), Identifier(s2)) => match ram {
            None => {
                if s != s2 {
                    Plus(
                        Box::from(Var(Box::from(Int(1)), 1, s)),
                        Box::from(Var(Box::from(Int(-1)), 1, s2)),
                    )
                } else {
                    Int(0)
                }
            }
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, minus),
        },
        (Identifier(s), Int(i)) => match ram {
            None => Plus(Box::from(Identifier(s.clone())), Box::from(Int(-i))),
            Some(_) => apply_operator_reverse(Int(i), Identifier(s), ram, minus),
        },
        (Null, Identifier(s)) => match ram {
            None => Var(Box::from(Int(-1)), 1, s),
            Some(_) => apply_operator(Identifier(s), Null, ram, minus),
        },
        (Identifier(s), Null) => match ram {
            None => Var(Box::from(Int(-1)), 1, s),
            Some(_) => apply_operator_reverse(Identifier(s), Null, ram, minus),
        },
        (Rational(r), Identifier(ss)) => match ram {
            None => Plus(
                Box::from(Rational(r)),
                Box::from(Var(Box::from(Int(-1)), 1, ss)),
            ),
            Some(_) => apply_operator_reverse(Rational(r), Identifier(ss.clone()), ram, minus),
        },
        (Identifier(ss), Rational(r)) => match ram {
            None => Plus(
                Box::from(Var(Box::from(Int(1)), 1, ss)),
                Box::from(Rational(r.opposite())),
            ),
            Some(_) => apply_operator(Identifier(ss), Rational(r), ram, minus),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Plus(Box::from(Int(i)), Box::from(Var(Box::from(Int(-1)), 1, s))),
            Some(_) => {
                let v = apply_operator(Identifier(s), Int(i), ram, minus);
                match v {
                    Int(i) => Int(-i),
                    p => minus(Int(0), p, None),
                }
            }
        },
        (Identifier(s), Float(i)) => match ram {
            None => Plus(
                Box::from(Var(Box::from(Int(1)), 1, s)),
                Box::from(Float(-i)),
            ),
            Some(_) => apply_operator(Identifier(s), Float(i), ram, minus),
        },
        (Float(i), Identifier(s)) => match ram {
            None => Plus(
                Box::from(Float(i)),
                Box::from(Var(Box::from(Int(-1)), 1, s)),
            ),
            Some(_) => {
                let v = apply_operator(Identifier(s), Float(i), ram, minus);
                match v {
                    Float(i) => Float(-i),
                    _ => Null,
                }
            }
        },

        (InterpreterVector(vec), Identifier(s)) => match ram {
            None => InterpreterVector(vec.clone()),
            Some(_) => {
                apply_operator_reverse(InterpreterVector(vec.clone()), Identifier(s), ram, minus)
            }
        },
        (Identifier(s), InterpreterVector(vec)) => match ram {
            None => InterpreterVector(vec.clone()),
            Some(_) => apply_operator(Identifier(s), InterpreterVector(vec.clone()), ram, minus),
        },
        (Bool(b), Identifier(s)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, minus),
        },
        (Identifier(s), Bool(b)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, minus),
        },
        (Plus(s1, s2), Plus(s3, s4)) => {
            let first = Plus(
                Box::from(minus(*s1.clone(), *s3.clone(), ram)),
                Box::from(minus(*s2.clone(), *s4.clone(), ram)),
            );
            let second = Plus(
                Box::from(minus(*s1.clone(), *s4.clone(), ram)),
                Box::from(minus(*s2.clone(), *s3.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));

            if s1 > s2 {
                second
            } else {
                first
            }
        }
        (Plus(s1, s2), Identifier(s3)) => {
            let first = Plus(
                Box::from(minus(*s1.clone(), Identifier(s3.clone()), ram)),
                s2.clone(),
            );
            let second = Plus(
                s1.clone(),
                Box::from(minus(*s2.clone(), Identifier(s3.clone()), ram)),
            );
            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Identifier(s3), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(minus(Identifier(s3.clone()), *s1.clone(), ram)),
                Box::from(minus(Int(0), *s2.clone(), ram)),
            );
            let second = Plus(
                Box::from(minus(Int(0), *s1.clone(), ram)),
                Box::from(minus(Identifier(s3.clone()), *s2.clone(), ram)),
            );
            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Int(i), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(minus(Int(i), *s1.clone(), ram)),
                Box::from(minus(Int(0), *s2.clone(), ram)),
            );
            let second = Plus(
                Box::from(minus(Int(0), *s1.clone(), ram)),
                Box::from(minus(Int(i), *s2.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Int(i)) => {
            let first = Plus(Box::from(minus(*s1.clone(), Int(i), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(minus(*s2.clone(), Int(i), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Float(f)) => {
            let first = Plus(Box::from(minus(*s1.clone(), Float(f), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(minus(*s2.clone(), Float(f), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Float(f), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(minus(Float(f), *s1.clone(), ram)),
                Box::from(minus(Int(0), *s2.clone(), ram)),
            );
            let second = Plus(
                Box::from(minus(Int(0), *s1.clone(), ram)),
                Box::from(minus(Float(f), *s2.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Rational(r)) => {
            let first = Plus(Box::from(minus(*s1.clone(), Rational(r), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(minus(*s2.clone(), Rational(r), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Rational(r), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(minus(Rational(r), *s1.clone(), ram)),
                Box::from(minus(Int(0), *s2.clone(), ram)),
            );
            let second = Plus(
                Box::from(minus(Int(0), *s1.clone(), ram)),
                Box::from(minus(Rational(r), *s2.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Var(x, y, z)) => {
            let first = Plus(
                Box::from(minus(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
                s2.clone(),
            );
            let second = Plus(
                s1.clone(),
                Box::from(minus(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Var(x, y, z), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(minus(Var(x.clone(), y, z.clone()), *s1.clone(), ram)),
                Box::from(minus(Int(0), *s2.clone(), ram)),
            );
            let second = Plus(
                Box::from(minus(Int(0), *s1.clone(), ram)),
                Box::from(minus(Var(x.clone(), y, z.clone()), *s2.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));

            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Null, Plus(s1, s2)) => Plus(
            Box::from(minus(Int(0), *s1.clone(), ram)),
            Box::from(minus(Int(0), *s2.clone(), ram)),
        ),

        (Plus(s1, s2), Null) => Plus(
            Box::from(minus(Int(0), *s1.clone(), ram)),
            Box::from(minus(Int(0), *s2.clone(), ram)),
        ),

        (Mul(s1, s2), Mul(s3, s4)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Mul(Box::from(minus(Int(0), *s3.clone(), ram)), s4.clone())),
        ),

        (Mul(s1, s2), Identifier(s)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Var(Box::from(Int(-1)), 1, s.clone())),
        ),

        (Identifier(s), Mul(s1, s2)) => Plus(
            Box::from(Var(Box::from(Int(1)), 1, s.clone())),
            Box::from(Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone())),
        ),

        (Mul(s1, s2), Int(i)) => Plus(Box::from(Mul(s1.clone(), s2.clone())), Box::from(Int(-i))),

        (Int(i), Mul(s1, s2)) => Plus(
            Box::from(Int(i)),
            Box::from(Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone())),
        ),

        (Mul(s1, s2), Float(f)) => {
            Plus(Box::from(Mul(s1.clone(), s2.clone())), Box::from(Float(-f)))
        }

        (Float(f), Mul(s1, s2)) => Plus(
            Box::from(Float(f)),
            Box::from(Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone())),
        ),

        (Mul(s1, s2), Rational(r)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Rational(r.opposite())),
        ),

        (Rational(r), Mul(s1, s2)) => Plus(
            Box::from(Rational(r)),
            Box::from(Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone())),
        ),

        (Var(x, y, z), Mul(s1, s2)) => Plus(
            Box::from(Var(x.clone(), y, z.clone())),
            Box::from(Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone())),
        ),

        (Mul(s1, s2), Var(x, y, z)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Var(Box::from(minus(Int(0), *x.clone(), ram)), y, z.clone())),
        ),

        (Mul(s1, s2), Plus(s3, s4)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Plus(
                Box::from(minus(Int(0), *s3.clone(), ram)),
                Box::from(minus(Int(0), *s4.clone(), ram)),
            )),
        ),

        (Plus(s3, s4), Mul(s1, s2)) => Plus(
            Box::from(Plus(s3.clone(), s4.clone())),
            Box::from(Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone())),
        ),

        (Null, Mul(s1, s2)) => Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone()),

        (Mul(s1, s2), Null) => Mul(Box::from(minus(Int(0), *s1.clone(), ram)), s2.clone()),

        (Var(x, y, z), Var(x1, y1, z1)) => {
            if z == z1 && y == y1 {
                Var(Box::from(minus(*x.clone(), *x1.clone(), ram)), y, z)
            } else {
                Plus(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(
                        Box::from(minus(Int(0), *x1.clone(), ram)),
                        y1.clone(),
                        z1.clone(),
                    )),
                )
            }
        }

        (Var(x, y, z), Identifier(s)) => {
            if z == s && y == 1 {
                Var(Box::from(minus(*x.clone(), Int(1), ram)), y, z)
            } else {
                Plus(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(Box::from(Int(-1)), 1, s.clone())),
                )
            }
        }

        (Identifier(s), Var(x, y, z)) => {
            if z == s && y == 1 {
                Var(Box::from(minus(Int(1), *x.clone(), ram)), y, z)
            } else {
                Plus(
                    Box::from(Var(
                        Box::from(minus(Int(0), *x.clone(), ram)),
                        y.clone(),
                        z.clone(),
                    )),
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                )
            }
        }

        (Int(i), Var(x, y, z)) => Plus(
            Box::from(Int(i)),
            Box::from(Var(Box::from(minus(Int(0), *x.clone(), ram)), y, z)),
        ),

        (Var(x, y, z), Int(i)) => Plus(Box::from(Var(x, y, z)), Box::from(Int(-i))),

        (Float(f), Var(x, y, z)) => Plus(
            Box::from(Float(f)),
            Box::from(Var(Box::from(minus(Int(0), *x.clone(), ram)), y, z)),
        ),

        (Var(x, y, z), Float(f)) => Plus(Box::from(Var(x, y, z)), Box::from(Float(-f))),

        (Rational(r), Var(x, y, z)) => Plus(
            Box::from(Rational(r)),
            Box::from(Var(Box::from(minus(Int(0), *x.clone(), ram)), y, z)),
        ),

        (Var(x, y, z), Rational(r)) => {
            Plus(Box::from(Var(x, y, z)), Box::from(Rational(r.opposite())))
        }

        (Var(x, y, z), Div(s1, s2)) => {
            let first = Div(
                Box::from(minus(
                    mult(Var(x.clone(), y, z.clone()), *s2.clone(), ram),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Var(x, y, z)) => {
            let first = Div(
                Box::from(minus(
                    *s1.clone(),
                    mult(*s2.clone(), Var(x.clone(), y, z.clone()), ram),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Mul(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(minus(
                    mult(*s4.clone(), mult(*s1.clone(), *s2.clone(), ram), ram),
                    *s3.clone(),
                    ram,
                )),
                s4.clone(),
            );
            first
        }

        (Div(s1, s2), Mul(s3, s4)) => {
            let first = Div(
                Box::from(minus(
                    *s3.clone(),
                    mult(*s4.clone(), mult(*s1.clone(), *s2.clone(), ram), ram),
                    ram,
                )),
                s4.clone(),
            );
            first
        }

        (Div(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(minus(
                    mult(*s1.clone(), *s4.clone(), ram),
                    mult(*s2.clone(), *s3.clone(), ram),
                    ram,
                )),
                Box::from(mult(*s2.clone(), *s4.clone(), ram)),
            );
            first
        }

        (Div(s1, s2), Identifier(s)) => {
            let first = Div(
                Box::from(minus(
                    *s1.clone(),
                    mult(
                        *s2.clone(),
                        Var(Box::from(Parameters::Int(1)), 1, s.clone()),
                        ram,
                    ),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Identifier(s), Div(s1, s2)) => {
            let first = Div(
                Box::from(minus(
                    mult(
                        *s2.clone(),
                        Var(Box::from(Parameters::Int(1)), 1, s.clone()),
                        ram,
                    ),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Int(i)) => {
            let first = Div(Box::from(mult(*s1.clone(), Int(i), ram)), s2.clone());
            first
        }

        (Int(i), Div(s1, s2)) => {
            let first = Div(Box::from(mult(Int(i), *s1.clone(), ram)), s2.clone());
            first
        }

        (Div(s1, s2), Float(f)) => {
            let first = Div(Box::from(mult(*s1.clone(), Float(f), ram)), s2.clone());
            first
        }

        (Float(f), Div(s1, s2)) => {
            let first = Div(Box::from(mult(Float(f), *s1.clone(), ram)), s2.clone());
            first
        }

        (Div(s1, s2), Rational(r)) => {
            let first = Div(
                Box::from(minus(*s1.clone(), mult(*s2.clone(), Rational(r), ram), ram)),
                s2.clone(),
            );
            first
        }

        (Rational(r), Div(s1, s2)) => {
            let first = Div(
                Box::from(minus(mult(Rational(r), *s2.clone(), ram), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        //x/y - a+b = x - y(a+b)/y
        (Div(s1, s2), Plus(s3, s4)) => {
            let first = Div(
                Box::from(minus(
                    *s1.clone(),
                    mult(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Plus(s3, s4), Div(s1, s2)) => {
            let first = Div(
                Box::from(minus(
                    mult(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                    *s1.clone(),
                    ram,
                )),
                s1.clone(),
            );
            first
        }

        (Null, Div(s1, s2)) => divide(
            minus(Parameters::Int(0), *s1.clone(), ram),
            *s2.clone(),
            ram,
        ),

        (Div(s1, s2), Null) => divide(
            minus(Parameters::Int(0), *s1.clone(), ram),
            *s2.clone(),
            ram,
        ),

        _ => Identifier("Those two values are incompatible with the - operator".to_string()),
    }
}
