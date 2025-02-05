use crate::functions::function::apply_operator;
use crate::functions::function::apply_operator_reverse;
use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;

use super::add::ORam;

pub fn expo(i: Parameters, i2: Parameters, ram: ORam) -> Parameters {
    match (i, i2) {
        (Null, Int(v)) => Int(v),
        (Null, Float(f)) => Float(f),
        (Int(v), Null) => Int(v),
        (Float(f), Null) => Float(f),
        (Int(v), Int(v2)) => Float((v as f64).powf(v2 as f64)),
        (Int(v), Float(f)) => Float((v as f64).powf(f)),
        (Float(v), Float(f)) => Float(v.powf(f)),
        (Float(v), Int(i1)) => Float(v.powf(i1 as f64)),

        (Rational(s), Null) => Rational(s.clone()),
        (Null, Rational(s)) => Rational(s.clone()),
        (Rational(s), Rational(s2)) => Float(s.approx().powf(s2.approx())),
        (Rational(s), Int(i)) => Float(s.approx().powf(i as f64)),
        (Int(i), Rational(s)) => Float((i as f64).powf(s.approx())),
        (Rational(s), Float(f)) => Float(s.approx().powf(f)),
        (Float(f), Rational(s)) => Float(f.powf(s.approx())),
        (Bool(_), Int(i)) => Int(i),
        (Bool(_), Float(i)) => Float(i),
        (Int(i), Bool(_)) => Int(i),
        (Float(i), Bool(_)) => Float(i),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Identifier(s), Identifier(s2)) => match ram {
            None => Identifier(s),
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, expo),
        },
        (Identifier(s), Int(i)) => match ram {
            None => Int(i),
            Some(_) => apply_operator(Identifier(s), Int(i), ram, expo),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Int(i),
            Some(_) => apply_operator_reverse(Int(i), Identifier(s), ram, expo),
        },
        (Identifier(s), Float(i)) => match ram {
            None => Float(i),
            Some(_) => apply_operator(Identifier(s), Float(i), ram, expo),
        },

        (Rational(s), Identifier(ss)) => match ram {
            None => Rational(s),
            Some(_) => {
                apply_operator_reverse(Rational(s.clone()), Identifier(ss.clone()), ram, expo)
            }
        },
        (Identifier(ss), Rational(s)) => match ram {
            None => Rational(s),
            Some(_) => apply_operator(Identifier(ss), Rational(s), ram, expo),
        },
        (Identifier(s), Null) => match ram {
            None => Null,
            Some(_) => apply_operator(Identifier(s), Null, ram, expo),
        },
        (Null, Identifier(s)) => match ram {
            None => Null,
            Some(_) => apply_operator(Identifier(s), Null, ram, expo),
        },
        (Float(i), Identifier(s)) => match ram {
            None => Float(i),
            Some(_) => apply_operator_reverse(Float(i), Identifier(s), ram, expo),
        },
        (Bool(b), Identifier(s)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, expo),
        },
        (Identifier(s), Bool(b)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, expo),
        },

        _ => Identifier("@Those two values are incompatible with the ^ operator".to_string()),
    }
}
