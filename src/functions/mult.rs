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
        (Null, Int(v)) => Int(v),
        (Null, Float(f)) => Float(f),
        (Int(v), Null) => Int(v),
        (Float(f), Null) => Float(f),
        (Int(v), Int(v2)) => Int(v * v2),
        (Int(v), Float(f)) => Float((v as f64) * f),
        (Float(v), Float(f)) => Float(v * f),
        (Float(v), Int(i1)) => Float(v * (i1 as f64)),

        (Rational(s), Null) => Rational(s.clone()),
        (Null, Rational(s)) => Rational(s.clone()),
        (Rational(s), Rational(s2)) => Rational(s * s2),
        (Rational(s), Int(i)) => Rational(s * Rationals::new(1, i)),
        (Int(i), Rational(s)) => Rational(s * Rationals::new(1, i)),
        (Rational(s), Float(f)) => Float(s.approx() * f),
        (Float(f), Rational(s)) => Float(f * s.approx()),
        (Null, InterpreterVector(vec)) => InterpreterVector(vec.clone()),
        (InterpreterVector(vec), Null) => InterpreterVector(vec.clone()),
        (InterpreterVector(vec), Int(v)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Int(v), ram))
                .for_each(|x| result.push(x));
            InterpreterVector(Box::from(result))
        }
        (Int(v), InterpreterVector(vec)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Int(v), ram))
                .for_each(|x| result.push(x));
            InterpreterVector(Box::from(result))
        }
        (InterpreterVector(vec), Float(v)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Float(v), ram))
                .for_each(|x| result.push(x));
            InterpreterVector(Box::from(result))
        }
        (Float(v), InterpreterVector(vec)) => {
            let mut result = Vec::new();
            vec.into_iter()
                .map(|x| mult(x.clone(), Float(v), ram))
                .for_each(|x| result.push(x));
            InterpreterVector(Box::from(result))
        }

        (InterpreterVector(vec), InterpreterVector(vec2)) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            let mut res2 = Vec::new();

            vec.clone().into_iter().for_each(|x| match x {
                InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });
            vec2.clone().into_iter().for_each(|x| match x {
                InterpreterVector(l) => res2.push(l.to_vec()),
                _ => {
                    is_matrix = false;
                }
            });

            if !is_matrix {
                let mut sum = Null;
                (*vec)
                    .into_iter()
                    .zip(vec2.into_iter())
                    .map(|(a, b)| mult(a.clone(), b.clone(), ram))
                    .for_each(|x| sum = add(sum.clone(), x, ram));

                match sum {
                    Int(i) => Int(i),
                    Float(f) => Float(f),
                    _ => Float(f64::NAN),
                }
            } else {
                let matrix_result = mult_matrix(res, res2, ram);

                let mut res = Vec::new();

                if matrix_result.len() == 0 {
                    return Null;
                }

                matrix_result
                    .into_iter()
                    .for_each(|x| res.push(InterpreterVector(Box::from(x))));

                InterpreterVector(Box::from(res))
            }
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
                if s == s2 {
                    Var(Box::from(Int(1)), 2, s.clone())
                } else {
                    Mul(
                        Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                        Box::from(Var(Box::from(Int(1)), 1, s2.clone())),
                    )
                }
            }
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, mult),
        },
        (Identifier(s), Int(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Int(i), ram, mult),
            None => Var(Box::from(Int(i)), 1, s.clone()),
        },

        (Rational(r), Identifier(ss)) => match ram {
            None => Var(Box::from(Rational(r)), 1, ss.clone()),
            Some(_) => apply_operator_reverse(Rational(r), Identifier(ss.clone()), ram, mult),
        },
        (Identifier(ss), Rational(r)) => match ram {
            Some(_) => apply_operator(Identifier(ss), Rational(r), ram, mult),
            None => Var(Box::from(Rational(r)), 1, ss.clone()),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Var(Box::from(Int(i)), 1, s.clone()),
            Some(_) => apply_operator(Identifier(s), Int(i), ram, mult),
        },
        (Identifier(s), InterpreterVector(vec)) => match ram {
            None => InterpreterVector(vec.clone()),
            Some(_) => apply_operator(Identifier(s), InterpreterVector(vec.clone()), ram, mult),
        },
        (InterpreterVector(vec), Identifier(s)) => match ram {
            None => InterpreterVector(vec.clone()),
            Some(_) => {
                apply_operator_reverse(InterpreterVector(vec.clone()), Identifier(s), ram, mult)
            }
        },
        (Null, Identifier(s)) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, mult),
            None => Var(Box::from(Int(1)), 1, s.clone()),
        },
        (Identifier(s), Null) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, mult),
            None => Var(Box::from(Int(1)), 1, s.clone()),
        },
        (Identifier(s), Float(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Float(i), ram, mult),
            None => Var(Box::from(Float(i)), 1, s.clone()),
        },
        (Float(i), Identifier(s)) => match ram {
            Some(_) => apply_operator(Identifier(s), Float(i), ram, mult),
            None => Var(Box::from(Float(i)), 1, s.clone()),
        },
        (Bool(b), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, mult),
            None => Bool(b),
        },
        (Identifier(s), Bool(b)) => match ram {
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, mult),
            None => Bool(b),
        },
        (Plus(s1, s2), Plus(s3, s4)) => {
            let first = Plus(
                Box::from(add(
                    mult(*s1.clone(), *s3.clone(), ram),
                    mult(*s2.clone(), *s3.clone(), ram),
                    ram,
                )),
                Box::from(add(
                    mult(*s1.clone(), *s4.clone(), ram),
                    mult(*s2.clone(), *s4.clone(), ram),
                    ram,
                )),
            );
            let second = Plus(
                Box::from(add(
                    mult(*s1.clone(), *s3.clone(), ram),
                    mult(*s2.clone(), *s4.clone(), ram),
                    ram,
                )),
                Box::from(add(
                    mult(*s2.clone(), *s3.clone(), ram),
                    mult(*s1.clone(), *s4.clone(), ram),
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
        (Plus(s1, s2), Identifier(s3)) => {
            let first = Plus(
                Box::from(mult(
                    *s1.clone(),
                    Var(Box::from(Int(1)), 1, s3.clone()),
                    ram,
                )),
                Box::from(mult(
                    *s2.clone(),
                    Var(Box::from(Int(1)), 1, s3.clone()),
                    ram,
                )),
            );
            first
        }
        (Identifier(s3), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(mult(Var(Box::new(Int(1)), 1, s3.clone()), *s1.clone(), ram)),
                Box::from(mult(Var(Box::new(Int(1)), 1, s3.clone()), *s2.clone(), ram)),
            );
            first
        }

        (Int(i), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(mult(Int(i), *s1.clone(), ram)),
                Box::from(mult(Int(i), *s2.clone(), ram)),
            );
            first
        }

        (Plus(s1, s2), Int(i)) => {
            let first = Plus(
                Box::from(mult(*s1.clone(), Int(i), ram)),
                Box::from(mult(*s2.clone(), Int(i), ram)),
            );
            first
        }

        (Float(f), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(mult(Float(f), *s1.clone(), ram)),
                Box::from(mult(Float(f), *s2.clone(), ram)),
            );
            first
        }

        (Plus(s1, s2), Float(f)) => {
            let first = Plus(
                Box::from(mult(*s1.clone(), Float(f), ram)),
                Box::from(mult(*s2.clone(), Float(f), ram)),
            );
            first
        }
        (Rational(r), Plus(s1, s2)) => {
            let first = Plus(
                Box::from(mult(Rational(r), *s1.clone(), ram)),
                Box::from(mult(Rational(r), *s2.clone(), ram)),
            );
            first
        }

        (Plus(s1, s2), Rational(r)) => {
            let first = Plus(
                Box::from(mult(*s1.clone(), Rational(r), ram)),
                Box::from(mult(*s2.clone(), Rational(r), ram)),
            );
            first
        }

        (Var(x, y, z), Plus(s1, s2)) => Plus(
            Box::from(mult(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
            Box::from(mult(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
        ),

        (Plus(s1, s2), Var(x, y, z)) => Plus(
            Box::from(mult(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
            Box::from(mult(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
        ),

        (Null, Plus(s1, s2)) => Plus(s1.clone(), s2.clone()),

        (Plus(s1, s2), Null) => Plus(s1.clone(), s2.clone()),

        (Var(x, y, z), Mul(s1, s2)) => {
            let first = Mul(
                Box::from(mult(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
                s2.clone(),
            );
            let second = Mul(
                s1.clone(),
                Box::from(mult(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Var(x, y, z)) => {
            let first = Mul(
                Box::from(mult(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
                s2.clone(),
            );
            let second = Mul(
                s1.clone(),
                Box::from(mult(*s2.clone(), Var(x.clone(), y, z.clone()), ram)),
            );

            let (ss1, ss2) = (size(&first), size(&second));

            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Mul(s3, s4)) => {
            let first = Mul(
                Box::from(mult(*s1.clone(), *s3.clone(), ram)),
                Box::from(mult(*s2.clone(), *s4.clone(), ram)),
            );
            let second = Mul(
                Box::from(mult(*s1.clone(), *s4.clone(), ram)),
                Box::from(mult(*s2.clone(), *s3.clone(), ram)),
            );

            let (ss1, ss2) = (size(&first), size(&second));
            if ss1 > ss2 {
                second
            } else {
                first
            }
        }

        (Mul(s1, s2), Identifier(s)) => Mul(
            Box::from(mult(*s1.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram)),
            s2.clone(),
        ),

        (Identifier(s), Mul(s1, s2)) => Mul(
            Box::from(mult(*s1.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram)),
            s2.clone(),
        ),

        (Mul(s1, s2), Int(i)) => Mul(Box::from(mult(*s1.clone(), Int(i), ram)), s2.clone()),

        (Int(i), Mul(s1, s2)) => Mul(Box::from(mult(*s1.clone(), Int(i), ram)), s2.clone()),

        (Mul(s1, s2), Float(f)) => Mul(Box::from(mult(*s1.clone(), Float(f), ram)), s2.clone()),

        (Float(f), Mul(s1, s2)) => Mul(Box::from(mult(*s1.clone(), Float(f), ram)), s2.clone()),

        (Mul(s1, s2), Rational(r)) => {
            Mul(Box::from(mult(*s1.clone(), Rational(r), ram)), s2.clone())
        }

        (Rational(r), Mul(s1, s2)) => {
            Mul(Box::from(mult(*s1.clone(), Rational(r), ram)), s2.clone())
        }

        (Mul(s1, s2), Plus(s3, s4)) => Plus(
            Box::from(mult(mult(*s1.clone(), *s3.clone(), ram), *s2.clone(), ram)),
            Box::from(mult(mult(*s1.clone(), *s4.clone(), ram), *s2.clone(), ram)),
        ),

        (Plus(s3, s4), Mul(s1, s2)) => Plus(
            Box::from(mult(mult(*s1.clone(), *s3.clone(), ram), *s2.clone(), ram)),
            Box::from(mult(mult(*s1.clone(), *s4.clone(), ram), *s2.clone(), ram)),
        ),

        (Null, Mul(s1, s2)) => Mul(s1.clone(), s2.clone()),

        (Mul(s1, s2), Null) => Mul(s1.clone(), s2.clone()),

        (Var(x, y, z), Var(x1, y1, z1)) => {
            if z == z1 {
                Var(Box::from(mult(*x.clone(), *x1.clone(), ram)), y + y1, z)
            } else {
                Mul(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(
                        Box::from(mult(Int(1), *x1.clone(), ram)),
                        y1.clone(),
                        z1.clone(),
                    )),
                )
            }
        }

        (Var(x, y, z), Identifier(s)) => {
            if z == s {
                Var(Box::from(x.clone()), y + 1, z)
            } else {
                Mul(
                    Box::from(Var(x.clone(), y.clone(), z.clone())),
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                )
            }
        }

        (Identifier(s), Var(x, y, z)) => {
            if z == s {
                Var(Box::from(x.clone()), y + 1, z)
            } else {
                Mul(
                    Box::from(Var(
                        Box::from(mult(Int(1), *x.clone(), ram)),
                        y.clone(),
                        z.clone(),
                    )),
                    Box::from(Var(Box::from(Int(1)), 1, s.clone())),
                )
            }
        }

        (Int(i), Var(x, y, z)) => Var(Box::from(mult(*x.clone(), Int(i), ram)), y, z.clone()),

        (Var(x, y, z), Int(i)) => Var(Box::from(mult(*x.clone(), Int(i), ram)), y, z.clone()),

        (Float(f), Var(x, y, z)) => Var(Box::from(mult(Float(f), *x.clone(), ram)), y, z.clone()),

        (Var(x, y, z), Float(f)) => Var(Box::from(mult(*x.clone(), Float(f), ram)), y, z.clone()),

        (Rational(r), Var(x, y, z)) => {
            Var(Box::from(mult(Rational(r), *x.clone(), ram)), y, z.clone())
        }

        (Var(x, y, z), Rational(r)) => {
            Var(Box::from(mult(*x.clone(), Rational(r), ram)), y, z.clone())
        }

        (Var(x, y, z), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(Var(x.clone(), y, z.clone()), *s1.clone(), ram)),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Var(x, y, z)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), Var(x.clone(), y, z.clone()), ram)),
                s2.clone(),
            );
            first
        }

        (Mul(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(mult(*s3.clone(), mult(*s1.clone(), *s2.clone(), ram), ram)),
                s4.clone(),
            );
            first
        }

        (Div(s1, s2), Mul(s3, s4)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), mult(*s3.clone(), *s4.clone(), ram), ram)),
                s2.clone(),
            );
            first
        }

        (Div(s1, s2), Div(s3, s4)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), *s3.clone(), ram)),
                Box::from(mult(*s2.clone(), *s4.clone(), ram)),
            );
            first
        }

        (Div(s1, s2), Identifier(s)) => {
            let first = Div(
                Box::from(mult(*s1.clone(), Var(Box::from(Int(1)), 1, s.clone()), ram)),
                s2.clone(),
            );
            first
        }

        (Identifier(s), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(Var(Box::from(Int(1)), 1, s.clone()), *s1.clone(), ram)),
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
                Box::from(mult(*s1.clone(), Int(r.over), ram)),
                Box::from(mult(*s2.clone(), Int(r.under), ram)),
            );
            first
        }

        (Rational(r), Div(s1, s2)) => {
            let first = Div(
                Box::from(mult(Int(r.over), *s1.clone(), ram)),
                Box::from(mult(*s2.clone(), Int(r.under), ram)),
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
                Box::from(mult(*s1.clone(), add(*s3.clone(), *s4.clone(), ram), ram)),
                s2.clone(),
            );
            first
        }

        (Null, Div(s1, s2)) => Div(s1.clone(), s2.clone()),

        (Div(s1, s2), Null) => Div(s1.clone(), s2.clone()),

        (Call(x, y), Call(a, b)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Call(a.clone(), b.clone())),
        ),

        (Call(x, y), Null) => Call(x.clone(), y.clone()),

        (Null, Call(x, y)) => Call(x.clone(), y.clone()),

        (Call(x, y), Int(i)) => Mul(Box::from(Call(x.clone(), y.clone())), Box::from(Int(i))),

        (Call(x, y), Float(i)) => Mul(Box::from(Call(x.clone(), y.clone())), Box::from(Float(i))),

        (Call(x, y), Rational(i)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Rational(i)),
        ),

        (Int(i), Call(x, y)) => Mul(Box::from(Call(x.clone(), y.clone())), Box::from(Int(i))),

        (Float(i), Call(x, y)) => Mul(Box::from(Call(x.clone(), y.clone())), Box::from(Float(i))),

        (Rational(i), Call(x, y)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Rational(i)),
        ),

        (Call(x, y), Identifier(a)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Var(Box::from(Int(1)), 1, a.clone())),
        ),

        (Identifier(a), Call(x, y)) => Mul(
            Box::from(Var(Box::from(Int(1)), 1, a.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Call(x, y), Var(a, b, c)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Var(a.clone(), b, c.clone())),
        ),

        (Var(a, b, c), Call(x, y)) => Mul(
            Box::from(Var(a.clone(), b, c.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Call(x, y), Plus(a, b)) => Mul(
            Box::from(Plus(a.clone(), b.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Plus(a, b), Call(x, y)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Plus(a.clone(), b.clone())),
        ),

        (Call(x, y), Mul(a, b)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Mul(a.clone(), b.clone())),
        ),

        (Mul(a, b), Call(x, y)) => Mul(
            Box::from(Mul(a.clone(), b.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        (Call(x, y), Div(a, b)) => Mul(
            Box::from(Call(x.clone(), y.clone())),
            Box::from(Div(a.clone(), b.clone())),
        ),

        (Div(a, b), Call(x, y)) => Mul(
            Box::from(Div(a.clone(), b.clone())),
            Box::from(Call(x.clone(), y.clone())),
        ),

        _ => Identifier("@Those two values are incompatible with the * operator".to_string()),
    }
}
