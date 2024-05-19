use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters::*;
use crate::parsing::ast::*;
use std::collections::HashMap;

use super::add::add;
use super::mult::mult;

pub fn divide(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(v)) => Int(v),
        (Null, Float(f)) => Float(f),
        (Int(v), Null) => Int(v),
        (Float(f), Null) => Float(f),
        (Int(v), Int(v2)) => Rational(Rationals::new(v2, v)),
        (Int(v), Float(f)) => Float((v as f64) / f),
        (Float(v), Float(f)) => Float(v / f),
        (Float(v), Int(i1)) => Float(v / (i1 as f64)),
        (Null, InterpreterVector(vec)) => InterpreterVector(vec.clone()),
        (InterpreterVector(vec), Null) => InterpreterVector(vec.clone()),

        (Rational(s), Null) => Rational(Rationals::new(1, 1) / s),
        (Null, Rational(s)) => Rational(Rationals::new(1, 1) / s),
        (Rational(s), Rational(s2)) => Rational(s / s2),

        (Rational(s), Int(i)) => Rational(s / Rationals::new(1, i)),
        (Int(i), Rational(s)) => Rational(Rationals::new(1, i) / s),
        (Rational(s), Float(f)) => Float(s.approx() / f),
        (Float(f), Rational(s)) => Float(f / s.approx()),
        (Bool(_), Int(i)) => Int(i),
        (Bool(_), Float(i)) => Float(i),
        (Int(i), Bool(_)) => Int(i),
        (Float(i), Bool(_)) => Float(i),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Identifier(s), Identifier(s2)) => match ram {
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, divide),
            None => divide(
                Var(Box::from(Int(1)), 1, s.clone()),
                Var(Box::from(Int(1)), 1, s2.clone()),
                ram,
            ),
        },

        (Rational(r), Identifier(ss)) => match ram {
            Some(_) => apply_operator_reverse(Rational(r), Identifier(ss.clone()), ram, divide),
            None => Div(
                Box::from(Int(r.over)),
                Box::from(Var(Box::from(Int(r.under)), 1, ss.clone())),
            ),
        },
        (Identifier(ss), Rational(r)) => match ram {
            Some(_) => apply_operator(Identifier(ss), Rational(r), ram, divide),
            None => match r.invert() {
                Ok(r) => Var(Box::from(Rational(r)), 1, ss.clone()),
                Err(_) => Null,
            },
        },

        (Identifier(s), Int(i)) => match ram {
            None => Var(Box::new(Rational(Rationals::new(i, 1))), 1, s.clone()),
            Some(_) => apply_operator(Identifier(s), Int(i), ram, divide),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Div(
                Box::from(Int(i)),
                Box::from(Var(Box::from(Int(1)), 1, s.clone())),
            ),
            Some(_) => {
                let v = apply_operator(Identifier(s.clone()), Int(i), ram, divide);
                match v {
                    Float(i) => Float(1.0 / i),
                    _ => divide(Int(i), Identifier(s.clone()), None),
                }
            }
        },
        (Null, Identifier(s)) => match ram {
            None => Div(
                Box::new(Int(1)),
                Box::new(Var(Box::new(Int(1)), 1, s.clone())),
            ),
            Some(_) => apply_operator(Identifier(s), Null, ram, divide),
        },
        (Identifier(s), Null) => match ram {
            None => Div(
                Box::new(Int(1)),
                Box::new(Var(Box::new(Int(1)), 1, s.clone())),
            ),
            Some(_) => apply_operator(Identifier(s), Null, ram, divide),
        },
        (Identifier(s), Float(i)) => match ram {
            None => Var(
                Box::from(Rational(Rationals::rationalize(1.0 / i))),
                1,
                s.clone(),
            ),
            Some(_) => apply_operator(Identifier(s), Float(i), ram, divide),
        },
        (Float(i), Identifier(s)) => match ram {
            None => Div(
                Box::from(Int(Rationals::rationalize(i).over)),
                Box::from(Var(
                    Box::from(Int(Rationals::rationalize(i).under)),
                    1,
                    s.clone(),
                )),
            ),
            Some(_) => {
                let v = apply_operator(Identifier(s), Float(i), ram, divide);
                match v {
                    Float(i) => Float(1.0 / i),
                    _ => Null,
                }
            }
        },
        (Bool(b), Identifier(s)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, divide),
        },
        (Identifier(s), Bool(b)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, divide),
        },
        (Plus(s1, s2), Plus(s3, s4)) => {
            let first = add(
                divide(*s1.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                divide(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                ram,
            );
            first
        }
        (Plus(s1, s2), Identifier(s3)) => {
            let first = add(
                divide(*s1.clone(), Var(Box::from(Int(1)), 1, s3.clone()), ram),
                divide(*s2.clone(), Var(Box::from(Int(1)), 1, s3.clone()), ram),
                ram,
            );
            first
        }

        (Identifier(s3), Plus(s1, s2)) => {
            let first = Div(
                Box::from(Var(Box::new(Int(1)), 1, s3.clone())),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }
        (Int(i), Plus(s1, s2)) => {
            let first = Div(
                Box::from(Int(i)),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }

        (Plus(s1, s2), Int(i)) => {
            let first = add(
                divide(*s1.clone(), Int(i), ram),
                divide(*s2.clone(), Int(i), ram),
                ram,
            );
            first
        }

        (Float(f), Plus(s1, s2)) => {
            let first = Div(
                Box::from(Float(f)),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }

        (Plus(s1, s2), Float(f)) => {
            let first = add(
                divide(*s1.clone(), Float(f), ram),
                divide(*s2.clone(), Float(f), ram),
                ram,
            );
            first
        }
        (Rational(r), Plus(s1, s2)) => {
            let first = Div(
                Box::from(Rational(r)),
                Box::from(add(*s1.clone(), *s2.clone(), ram)),
            );
            first
        }

        (Plus(s1, s2), Rational(r)) => {
            let first = add(
                divide(*s1.clone(), Rational(r), ram),
                divide(*s2.clone(), Rational(r), ram),
                ram,
            );
            first
        }
        (Var(x, y, z), Plus(s1, s2)) => Div(
            Box::from(Var(x.clone(), y.clone(), z.clone())),
            Box::from(add(*s1.clone(), *s2.clone(), ram)),
        ),

        (Plus(s1, s2), Var(x, y, z)) => add(
            divide(*s1.clone(), Var(x.clone(), y, z.clone()), ram),
            divide(*s2.clone(), Var(x.clone(), y, z.clone()), ram),
            ram,
        ),

        (Null, Plus(s1, s2)) => add(*s1.clone(), *s2.clone(), ram),

        (Plus(s1, s2), Null) => add(*s1.clone(), *s2.clone(), ram),

        (Var(x, y, z), Mul(s1, s2)) => {
            let first = mult(
                divide(Var(x.clone(), y, z.clone()), *s1.clone(), ram),
                divide(Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Int(1), *s1.clone(), ram),
                divide(Var(x.clone(), y, z.clone()), *s2.clone(), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Var(x, y, z)) => {
            let first = mult(
                divide(*s1.clone(), Var(x.clone(), y, z.clone()), ram),
                *s2.clone(),
                ram,
            );
            let second = mult(
                *s1.clone(),
                divide(*s2.clone(), Var(x.clone(), y, z.clone()), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Mul(s3, s4)) => {
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

        (Mul(s1, s2), Identifier(s)) => {
            let first = mult(
                divide(*s1.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram),
                *s2.clone(),
                ram,
            );
            let second = mult(
                *s1.clone(),
                divide(*s2.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Identifier(s), Mul(s1, s2)) => {
            let first = mult(
                divide(Var(Box::from(Int(1)), 1, s.clone()), *s1.clone(), ram),
                divide(Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Int(1), *s1.clone(), ram),
                divide(Var(Box::from(Int(1)), 1, s.clone()), *s2.clone(), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Int(i)) => {
            let first = mult(divide(*s1.clone(), Int(i), ram), *s2.clone(), ram);
            let second = mult(*s1.clone(), divide(*s2.clone(), Int(i), ram), ram);

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Int(i), Mul(s1, s2)) => {
            let first = mult(
                divide(Int(i), *s1.clone(), ram),
                divide(Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Int(1), *s1.clone(), ram),
                divide(Int(i), *s2.clone(), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Float(f)) => {
            let first = mult(divide(*s1.clone(), Float(f), ram), *s2.clone(), ram);
            let second = mult(*s1.clone(), divide(*s2.clone(), Float(f), ram), ram);

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Float(f), Mul(s1, s2)) => {
            let first = mult(
                divide(Float(f), *s1.clone(), ram),
                divide(Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Int(1), *s1.clone(), ram),
                divide(Float(f), *s2.clone(), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Rational(r)) => {
            let first = mult(divide(*s1.clone(), Rational(r), ram), *s2.clone(), ram);
            let second = mult(*s1.clone(), divide(*s2.clone(), Rational(r), ram), ram);

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Rational(r), Mul(s1, s2)) => {
            let first = mult(
                divide(Rational(r), *s1.clone(), ram),
                divide(Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Int(1), *s1.clone(), ram),
                divide(Rational(r), *s2.clone(), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Plus(s3, s4)) => {
            let first = mult(
                divide(*s1.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                divide(Int(1), *s2.clone(), ram),
                ram,
            );
            let second = mult(
                divide(Int(1), *s1.clone(), ram),
                divide(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                ram,
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Plus(s3, s4), Mul(s1, s2)) => {
            let first = add(
                divide(*s3.clone(), mult(*s1.clone(), *s2.clone(), ram), ram),
                divide(*s4.clone(), mult(*s1.clone(), *s2.clone(), ram), ram),
                ram,
            );
            first
        }

        (Null, Mul(s1, s2)) => mult(*s1.clone(), *s2.clone(), ram),

        (Mul(s1, s2), Null) => mult(*s1.clone(), *s2.clone(), ram),

        (Var(x, y, z), Var(x1, y1, z1)) => {
            if z == z1 {
                Var(Box::from(divide(*x.clone(), *x1.clone(), ram)), y - y1, z)
            } else {
                Div(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(
                        Box::from(divide(Int(1), *x1.clone(), ram)),
                        y1.clone(),
                        z1.clone(),
                    )),
                )
            }
        }

        (Var(x, y, z), Identifier(s)) => {
            if z == s {
                Var(Box::from(x.clone()), y - 1, z)
            } else {
                Div(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                )
            }
        }

        (Identifier(s), Var(x, y, z)) => {
            if z == s {
                Var(Box::from(divide(Int(1), *x.clone(), ram)), 1 - y, z)
            } else {
                Div(
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                    Box::from(Var(
                        Box::from(mult(Int(1), *x.clone(), ram)),
                        y.clone(),
                        z.clone(),
                    )),
                )
            }
        }

        (Int(i), Var(x, y, z)) => Var(Box::from(divide(Int(i), *x.clone(), ram)), -y, z.clone()),

        (Var(x, y, z), Int(i)) => Var(Box::from(divide(*x.clone(), Int(i), ram)), y, z.clone()),

        (Float(f), Var(x, y, z)) => {
            Var(Box::from(divide(Float(f), *x.clone(), ram)), -y, z.clone())
        }

        (Var(x, y, z), Float(f)) => Var(Box::from(divide(*x.clone(), Float(f), ram)), y, z.clone()),

        (Rational(r), Var(x, y, z)) => Var(
            Box::from(divide(Rational(r), *x.clone(), ram)),
            -y,
            z.clone(),
        ),

        (Var(x, y, z), Rational(r)) => Var(
            Box::from(divide(*x.clone(), Rational(r), ram)),
            y,
            z.clone(),
        ),

        (Var(x, y, z), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(Var(x.clone(), y, z.clone()), *s2.clone(), ram)),
                s1.clone(),
            );
            first
        }

        (Div(s1, s2), Var(x, y, z)) => {
            let first = Div(
                s1.clone(),
                Box::from(mult(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
            );
            first
        }

        (Mul(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(mult(*s4.clone(), mult(*s1.clone(), *s2.clone(), ram), ram)),
                s3.clone(),
            );
            first
        }

        (Div(s1, s2), Mul(s3, s4)) => {
            let first = Div(
                s1.clone(),
                Box::from(mult(*s2.clone(), mult(*s3.clone(), *s4.clone(), ram), ram)),
            );
            first
        }

        (Div(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), *s4.clone(), ram)),
                Box::from(mult(*s2.clone(), *s3.clone(), ram)),
            );
            first
        }

        (Div(s1, s2), Identifier(s)) => {
            let first = Div(
                s1.clone(),
                Box::from(mult(*s2.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram)),
            );
            first
        }

        (Identifier(s), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(Var(Box::from(Int(1)), 1, s.clone()), *s2.clone(), ram)),
                s1.clone(),
            );
            first
        }

        (Div(s1, s2), Int(i)) => {
            let first = Div(s1.clone(), Box::from(mult(*s2.clone(), Int(i), ram)));
            first
        }

        (Int(i), Div(s1, s2)) => {
            let first = Div(Box::from(mult(Int(i), *s2.clone(), ram)), s1.clone());
            first
        }

        (Div(s1, s2), Float(f)) => {
            let first = Div(s1.clone(), Box::from(mult(*s2.clone(), Float(f), ram)));
            first
        }

        (Float(f), Div(s1, s2)) => {
            let first = Div(Box::from(mult(Float(f), *s2.clone(), ram)), s1.clone());
            first
        }

        (Div(s1, s2), Rational(r)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), Int(r.under), ram)),
                Box::from(mult(*s2.clone(), Int(r.over), ram)),
            );
            first
        }

        (Rational(r), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(Int(r.under), *s2.clone(), ram)),
                Box::from(mult(*s1.clone(), Int(r.over), ram)),
            );
            first
        }

        (Div(s1, s2), Plus(s3, s4)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), add(*s3.clone(), *s4.clone(), ram), ram)),
                s2.clone(),
            );
            first
        }

        (Plus(s3, s4), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram)),
                s1.clone(),
            );
            first
        }

        (Null, Div(s1, s2)) => divide(*s2.clone(), *s1.clone(), ram),

        (Div(s1, s2), Null) => divide(*s2.clone(), *s1.clone(), ram),
        _ => Identifier("@Those two values are incompatible with the / operator".to_string()),
    }
}
