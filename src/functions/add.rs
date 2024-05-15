use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters::*;
use crate::parsing::ast::*;
use std::collections::HashMap;

pub fn add(i: Parameters, i2: Parameters, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
    match (i, i2) {
        (Null, Int(v)) => Int(v),
        (Null, Float(f)) => Float(f),
        (Null, InterpreterVector(vec)) => InterpreterVector(vec.clone()),
        (InterpreterVector(vec), Null) => InterpreterVector(vec.clone()),
        (Int(v), Null) => Int(v),
        (Float(f), Null) => Float(f),
        (Rational(s), Null) => Rational(s.clone()),
        (Null, Rational(s)) => Rational(s.clone()),
        (Rational(s), Rational(s2)) => Rational(s + s2),
        (Rational(s), Int(i)) => Rational(s + Rationals::new(1, i)),
        (Int(i), Rational(s)) => Rational(s + Rationals::new(1, i)),
        (Rational(s), Float(f)) => Float(s.approx() + f),
        (Float(f), Rational(s)) => Float(f + s.approx()),
        (Int(v), Int(v2)) => Int(v + v2),
        (Int(v), Float(f)) => Float((v as f64) + f),
        (Float(v), Float(f)) => Float(v + f),
        (Float(v), Int(i1)) => Float(v + (i1 as f64)),
        (InterpreterVector(vec), InterpreterVector(vec2)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .zip(vec2.into_iter())
                .map(|(x, y)| add(x.clone(), y.clone(), ram))
                .for_each(|s| res.push(s));
            InterpreterVector(Box::from(res))
        }
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
                        Box::from(Identifier(s.clone())),
                        Box::from(Identifier(s2.clone())),
                    )
                } else {
                    Var(Box::from(Int(2)), 1, s.clone())
                }
            }
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, add),
        },
        (Identifier(s), Int(i)) => match ram {
            None => Plus(Box::from(Identifier(s.clone())), Box::from(Int(i))),
            Some(_) => apply_operator(Identifier(s), Int(i), ram, add),
        },
        (Null, Identifier(s)) => match ram {
            None => Identifier(s.clone()),
            Some(_) => apply_operator(Identifier(s), Null, ram, add),
        },
        (Rational(s), Identifier(ss)) => match ram {
            None => Plus(
                Box::from(Rational(s.clone())),
                Box::from(Identifier(ss.clone())),
            ),
            Some(_) => {
                apply_operator_reverse(Rational(s.clone()), Identifier(ss.clone()), ram, add)
            }
        },
        (Identifier(ss), Rational(s)) => match ram {
            None => Plus(
                Box::from(Identifier(ss.clone())),
                Box::from(Rational(s.clone())),
            ),
            Some(_) => apply_operator(Identifier(ss), Rational(s), ram, add),
        },
        (Identifier(s), Null) => match ram {
            None => Identifier(s.clone()),
            Some(_) => apply_operator(Identifier(s), Null, ram, add),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Plus(Box::from(Identifier(s)), Box::from(Int(i.clone()))),
            Some(_) => apply_operator(Identifier(s), Int(i), ram, add),
        },
        (Identifier(s), Float(i)) => match ram {
            None => Plus(Box::from(Identifier(s.clone())), Box::from(Float(i))),
            Some(_) => apply_operator(Identifier(s), Float(i), ram, add),
        },
        (Float(i), Identifier(s)) => match ram {
            None => Plus(Box::from(Identifier(s.clone())), Box::from(Float(i))),
            Some(_) => apply_operator(Identifier(s), Float(i), ram, add),
        },
        (Identifier(s), InterpreterVector(vec)) => match ram {
            None => Null,
            Some(_) => apply_operator(Identifier(s), InterpreterVector(vec.clone()), ram, add),
        },
        (InterpreterVector(vec), Identifier(s)) => match ram {
            None => Null,
            Some(_) => apply_operator(Identifier(s), InterpreterVector(vec.clone()), ram, add),
        },
        (Bool(b), Identifier(s)) => match ram {
            None => Null,
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, add),
        },
        (Identifier(s), Bool(b)) => match ram {
            None => Null,
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, add),
        },
        (Plus(s1, s2), Plus(s3, s4)) => {
            let first = Plus(
                Box::from(add(*s1.clone(), *s3.clone(), ram)),
                Box::from(add(*s2.clone(), *s4.clone(), ram)),
            );
            let second = Plus(
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
        (Plus(s1, s2), Identifier(s3)) => {
            let first = Plus(
                Box::from(add(*s1.clone(), Identifier(s3.clone()), ram)),
                s2.clone(),
            );
            let second = Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Identifier(s3.clone()), ram)),
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
                Box::from(add(*s1.clone(), Identifier(s3.clone()), ram)),
                s2.clone(),
            );
            let second = Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Identifier(s3.clone()), ram)),
            );
            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Int(i), Plus(s1, s2)) => {
            let first = Plus(Box::from(add(*s1.clone(), Int(i), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Int(i), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Int(i)) => {
            let first = Plus(Box::from(add(*s1.clone(), Int(i), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Int(i), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Float(f)) => {
            let first = Plus(Box::from(add(*s1.clone(), Float(f), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Float(f), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Float(f), Plus(s1, s2)) => {
            let first = Plus(Box::from(add(*s1.clone(), Float(f), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Float(f), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Rational(r)) => {
            let first = Plus(Box::from(add(*s1.clone(), Rational(r), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Rational(r), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Rational(r), Plus(s1, s2)) => {
            let first = Plus(Box::from(add(*s1.clone(), Rational(r), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Rational(r), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Var(x, y, z)) => {
            let first = Plus(
                Box::from(add(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
                s2.clone(),
            );
            let second = Plus(
                s1.clone(),
                Box::from(add(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
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
                Box::from(add(Var(x.clone(), y, z.clone()), *s1.clone(), ram)),
                s2.clone(),
            );
            let second = Plus(
                s1.clone(),
                Box::from(add(Var(x.clone(), y, z.clone()), *s2.clone(), ram)),
            );

            let (s1, s2) = (size(&first), size(&second));

            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Plus(s1, s2), Null) => Plus(s1.clone(), s2.clone()),

        (Null, Plus(s1, s2)) => Plus(s1.clone(), s2.clone()),

        (Mul(s1, s2), Mul(s3, s4)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Mul(s3.clone(), s4.clone())),
        ),

        (Mul(s1, s2), Identifier(s)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Var(Box::from(Int(1)), 1, s.clone())),
        ),

        (Identifier(s), Mul(s1, s2)) => Plus(
            Box::from(Var(Box::from(Int(1)), 1, s.clone())),
            Box::from(Mul(s1.clone(), s2.clone())),
        ),

        (Mul(s1, s2), Int(i)) => Plus(Box::from(Mul(s1.clone(), s2.clone())), Box::from(Int(i))),

        (Int(i), Mul(s1, s2)) => Plus(Box::from(Int(i)), Box::from(Mul(s1.clone(), s2.clone()))),

        (Mul(s1, s2), Float(f)) => {
            Plus(Box::from(Mul(s1.clone(), s2.clone())), Box::from(Float(f)))
        }

        (Float(f), Mul(s1, s2)) => {
            Plus(Box::from(Float(f)), Box::from(Mul(s1.clone(), s2.clone())))
        }

        (Mul(s1, s2), Rational(r)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Rational(r)),
        ),

        (Rational(r), Mul(s1, s2)) => Plus(
            Box::from(Rational(r)),
            Box::from(Mul(s1.clone(), s2.clone())),
        ),

        (Var(x, y, z), Mul(s1, s2)) => Plus(
            Box::from(Var(x.clone(), y, z.clone())),
            Box::from(Mul(s1.clone(), s2.clone())),
        ),

        (Mul(s1, s2), Var(x, y, z)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Var(x.clone(), y, z.clone())),
        ),

        (Mul(s1, s2), Plus(s3, s4)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Plus(s3.clone(), s4.clone())),
        ),

        (Plus(s3, s4), Mul(s1, s2)) => Plus(
            Box::from(Plus(s3.clone(), s4.clone())),
            Box::from(Mul(s1.clone(), s2.clone())),
        ),

        (Null, Mul(s1, s2)) => Mul(s1.clone(), s2.clone()),

        (Mul(s1, s2), Null) => Mul(s1.clone(), s2.clone()),

        (Var(x, y, z), Var(x1, y1, z1)) => {
            if z == z1 && y == y1 {
                Var(Box::from(add(*x.clone(), *x1.clone(), ram)), y, z)
            } else {
                Plus(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(x1.clone(), y1.clone(), z1.clone())),
                )
            }
        }

        (Var(x, y, z), Identifier(s)) => {
            if z == s && y == 1 {
                Var(Box::from(add(*x.clone(), Int(1), ram)), y, z)
            } else {
                Plus(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                )
            }
        }

        (Identifier(s), Var(x, y, z)) => {
            if z == s && y == 1 {
                Var(Box::from(add(*x.clone(), Int(1), ram)), y, z)
            } else {
                Plus(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                )
            }
        }

        (Int(i), Var(x, y, z)) => Plus(Box::from(Int(i)), Box::from(Var(x, y, z))),

        (Var(x, y, z), Int(i)) => Plus(Box::from(Var(x, y, z)), Box::from(Int(i))),

        (Float(f), Var(x, y, z)) => Plus(Box::from(Float(f)), Box::from(Var(x, y, z))),

        (Var(x, y, z), Float(f)) => Plus(Box::from(Var(x, y, z)), Box::from(Float(f))),

        (Rational(r), Var(x, y, z)) => Plus(Box::from(Rational(r)), Box::from(Var(x, y, z))),

        (Var(x, y, z), Rational(r)) => Plus(Box::from(Var(x, y, z)), Box::from(Rational(r))),

        _ => Identifier("@Those two values are incompatible with the + operator".to_string()),
    }
}
