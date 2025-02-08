use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast;
use crate::parsing::ast::Parameters::*;
use crate::parsing::ast::*;

use super::mult::mult;

pub type ORam<'a> = Option<&'a ast::Ram>;

pub fn add(i: Parameters, i2: Parameters, ram: ORam) -> Parameters {
    match (i, i2) {
        (Null, Int(v)) => Int(v),
        (Null, Float(f, mode)) => Float(f, mode),
        (Null, InterpreterVector(vec)) => InterpreterVector(vec.clone()),
        (InterpreterVector(vec), Null) => InterpreterVector(vec.clone()),
        (Int(v), Null) => Int(v),
        (Float(f, mode), Null) => Float(f, mode),
        (Rational(s), Null) => Rational(s.clone()),
        (Null, Rational(s)) => Rational(s.clone()),
        (Rational(s), Rational(s2)) => Rational(s + s2),
        (Rational(s), Int(i)) => Rational(s + Rationals::new(1, i)),
        (Int(i), Rational(s)) => Rational(s + Rationals::new(1, i)),
        (Rational(s), Float(f, mode)) => Float(s.approx() + f, mode),
        (Float(f, mode), Rational(s)) => Float(f + s.approx(), mode),
        (Int(v), Int(v2)) => Int(v + v2),
        (Int(v), Float(f, mode)) => Float((v as f64) + f, mode),
        (Float(v, mode), Float(f, _)) => Float(v + f, mode),
        (Float(v, mode), Int(i1)) => Float(v + (i1 as f64), mode),
        (InterpreterVector(vec), InterpreterVector(vec2)) => {
            let mut res = Vec::new();
            vec.into_iter()
                .zip(vec2.into_iter())
                .map(|(x, y)| add(x.clone(), y.clone(), ram))
                .for_each(|s| res.push(s));
            InterpreterVector(Box::from(res))
        }
        (Bool(_), Int(i)) => Int(i),
        (Bool(_), Float(i, mode)) => Float(i, mode),
        (Int(i), Bool(_)) => Int(i),
        (Float(i, mode), Bool(_)) => Float(i, mode),
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
        (Rational(r), Identifier(ss)) => match ram {
            None => Plus(Box::from(Rational(r)), Box::from(Identifier(ss.clone()))),
            Some(_) => apply_operator_reverse(Rational(r), Identifier(ss.clone()), ram, add),
        },
        (Identifier(ss), Rational(r)) => match ram {
            None => Plus(Box::from(Identifier(ss.clone())), Box::from(Rational(r))),
            Some(_) => apply_operator(Identifier(ss), Rational(r), ram, add),
        },
        (Identifier(s), Null) => match ram {
            None => Identifier(s.clone()),
            Some(_) => apply_operator(Identifier(s), Null, ram, add),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Plus(Box::from(Identifier(s)), Box::from(Int(i.clone()))),
            Some(_) => apply_operator(Identifier(s), Int(i), ram, add),
        },
        (Identifier(s), Float(i, mode)) => match ram {
            None => Plus(Box::from(Identifier(s.clone())), Box::from(Float(i, mode))),
            Some(_) => apply_operator(Identifier(s), Float(i, mode), ram, add),
        },
        (Float(i, mode), Identifier(s)) => match ram {
            None => Plus(Box::from(Identifier(s.clone())), Box::from(Float(i, mode))),
            Some(_) => apply_operator(Identifier(s), Float(i, mode), ram, add),
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

        (Plus(s1, s2), Float(f, mode)) => {
            let first = Plus(Box::from(add(*s1.clone(), Float(f, mode), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Float(f, mode), ram)));

            let (s1, s2) = (size(&first), size(&second));
            if s1 > s2 {
                second
            } else {
                first
            }
        }

        (Float(f, mode), Plus(s1, s2)) => {
            let first = Plus(Box::from(add(*s1.clone(), Float(f, mode), ram)), s2.clone());
            let second = Plus(s1.clone(), Box::from(add(*s2.clone(), Float(f, mode), ram)));

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

        (Mul(s1, s2), Float(f, mode)) => Plus(
            Box::from(Mul(s1.clone(), s2.clone())),
            Box::from(Float(f, mode)),
        ),

        (Float(f, mode), Mul(s1, s2)) => Plus(
            Box::from(Float(f, mode)),
            Box::from(Mul(s1.clone(), s2.clone())),
        ),

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

        (Float(f, mode), Var(x, y, z)) => Plus(Box::from(Float(f, mode)), Box::from(Var(x, y, z))),

        (Var(x, y, z), Float(f, mode)) => Plus(Box::from(Var(x, y, z)), Box::from(Float(f, mode))),

        (Rational(r), Var(x, y, z)) => Plus(Box::from(Rational(r)), Box::from(Var(x, y, z))),

        (Var(x, y, z), Rational(r)) => Plus(Box::from(Var(x, y, z)), Box::from(Rational(r))),

        (Var(x, y, z), Div(s1, s2)) => {
            let first = Div(
                Box::from(add(
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
                Box::from(add(
                    mult(Var(x.clone(), y, z.clone()), *s2.clone(), ram),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Mul(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(add(
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
                Box::from(add(
                    mult(*s2.clone(), mult(*s3.clone(), *s4.clone(), ram), ram),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(add(
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
                Box::from(add(
                    mult(*s2.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Identifier(s), Div(s1, s2)) => {
            let first = Div(
                Box::from(add(
                    mult(Var(Box::from(Int(1)), 1, s.clone()), *s1.clone(), ram),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Int(i)) => {
            let first = Div(
                Box::from(add(mult(*s2.clone(), Int(i), ram), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        (Int(i), Div(s1, s2)) => {
            let first = Div(
                Box::from(add(mult(Int(i), *s2.clone(), ram), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Float(f, mode)) => {
            let first = Div(
                Box::from(add(
                    mult(*s2.clone(), Float(f, mode), ram),
                    *s1.clone(),
                    ram,
                )),
                s2.clone(),
            );
            first
        }

        (Float(f, mode), Div(s1, s2)) => {
            let first = Div(
                Box::from(add(mult(Float(f), *s2.clone(), ram), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Rational(r)) => {
            let first = Div(
                Box::from(add(mult(*s2.clone(), Rational(r), ram), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        (Rational(r), Div(s1, s2)) => {
            let first = Div(
                Box::from(add(mult(Rational(r), *s2.clone(), ram), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Plus(s3, s4)) => {
            let first = Div(
                Box::from(add(
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
                Box::from(add(
                    mult(*s2.clone(), add(*s3.clone(), *s4.clone(), ram), ram),
                    *s1.clone(),
                    ram,
                )),
                s1.clone(),
            );
            first
        }

        (Null, Div(s1, s2)) => Div(s1.clone(), s2.clone()),

        (Div(s1, s2), Null) => Div(s1.clone(), s2.clone()),

        (Call(x, y), Call(a, b)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Call(a.clone(), b.clone())),
        ),

        (Call(x, y), Null) => Call(x.clone(), y.clone()),

        (Null, Call(x, y)) => Call(x.clone(), y.clone()),

        (Call(x, y), Int(i)) => Plus(Box::from(Call(x.clone(), y.clone())), Box::from(Int(i))),

        (Call(x, y), Float(i)) => Plus(Box::from(Call(x.clone(), y.clone())), Box::from(Float(i))),

        (Call(x, y), Rational(i)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Rational(i)),
        ),

        (Int(i), Call(x, y)) => Plus(Box::from(Call(x.clone(), y.clone())), Box::from(Int(i))),

        (Float(i), Call(x, y)) => Plus(Box::from(Call(x.clone(), y.clone())), Box::from(Float(i))),

        (Rational(i), Call(x, y)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Rational(i)),
        ),

        (Call(x, y), Identifier(a)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Var(Box::from(Int(1)), 1, a.clone())),
        ),

        (Identifier(a), Call(x, y)) => Plus(
            Box::from(Var(Box::from(Int(1)), 1, a.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Call(x, y), Var(a, b, c)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Var(a.clone(), b, c.clone())),
        ),

        (Var(a, b, c), Call(x, y)) => Plus(
            Box::from(Var(a.clone(), b, c.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Call(x, y), Plus(a, b)) => Plus(
            Box::from(Plus(a.clone(), b.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Plus(a, b), Call(x, y)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Plus(a.clone(), b.clone())),
        ),

        (Call(x, y), Mul(a, b)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Mul(a.clone(), b.clone())),
        ),

        (Mul(a, b), Call(x, y)) => Plus(
            Box::from(Mul(a.clone(), b.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Call(x, y), Div(a, b)) => Plus(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Div(a.clone(), b.clone())),
        ),

        (Div(a, b), Call(x, y)) => Plus(
            Box::from(Div(a.clone(), b.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        _ => Identifier("@Those two values are incompatible with the + operator".to_string()),
    }
}
