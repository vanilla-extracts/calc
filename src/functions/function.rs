use std::collections::HashMap;

use crate::exact_math::rationals::Rationals;

use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;

pub fn apply_operator(
    value: Parameters,
    value2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
    f: fn(Parameters, Parameters, Option<&HashMap<String, Parameters>>) -> Parameters,
) -> Parameters {
    let s = match value {
        Identifier(ref s) => s,
        _ => "",
    };
    if s == "" {
        return Null;
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
        Identifier(ref s) => s,
        _ => "",
    };
    if s == "" {
        return Null;
    }
    match ram {
        None => f(value.clone(), value2.clone(), None),
        Some(i_ram) => {
            let val3 = i_ram.get(s);
            match val3 {
                None => f(value.clone(), value2.clone(), None),
                Some(val) => f(value.clone(), val.clone(), ram),
            }
        }
    }
}

pub fn assign(s: Parameters, s2: Parameters) -> (String, Parameters) {
    match s {
        Identifier(s) => (s, s2),
        _ => ("".to_string(), s2),
    }
}

pub fn greater(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(_)) => Bool(true),
        (Null, Float(_)) => Bool(true),
        (Int(_), Null) => Bool(true),
        (Float(_), Null) => Bool(true),
        (Int(v), Int(v2)) => Bool(v > v2),
        (Int(v), Float(f)) => Bool((v as f64) > f),
        (Float(v), Float(f)) => Bool(v > f),
        (Float(v), Int(i1)) => Bool(v > (i1 as f64)),
        (Rational(_), Null) => Bool(true),
        (Null, Rational(_)) => Bool(true),
        (Rational(s), Rational(s2)) => Bool(s > s2),
        (Rational(s), Int(i)) => Bool(s > Rationals::new(1, i)),
        (Int(i), Rational(s)) => Bool(Rationals::new(1, i) > s),
        (Rational(s), Float(f)) => Bool(s.approx() > f),
        (Float(f), Rational(s)) => Bool(f > s.approx()),
        (Bool(b), Int(_)) => Bool(b),
        (Bool(b), Float(_)) => Bool(b),
        (Int(_), Bool(b)) => Bool(b),
        (Float(_), Bool(b)) => Bool(b),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Identifier(s), Identifier(s2)) => match ram {
            None => Identifier(s),
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, greater),
        },

        (Rational(s), Identifier(ss)) => match ram {
            None => Rational(s),
            Some(_) => {
                apply_operator_reverse(Rational(s.clone()), Identifier(ss.clone()), ram, greater)
            }
        },
        (Identifier(ss), Rational(s)) => match ram {
            Some(_) => apply_operator(Identifier(ss), Rational(s), ram, greater),
            None => Rational(s),
        },
        (Identifier(s), Int(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Int(i), ram, greater),
            None => Int(i),
        },
        (Null, Identifier(s)) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, greater),
            None => Identifier(s),
        },
        (Identifier(s), Null) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, greater),
            None => Identifier(s),
        },
        (Int(i), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Int(i), Identifier(s), ram, greater),
            None => Int(i),
        },
        (Identifier(s), Float(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Float(i), ram, greater),
            None => Float(i),
        },
        (Float(i), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Float(i), Identifier(s), ram, greater),
            None => Float(i),
        },
        (Bool(b), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, greater),
            None => Bool(b),
        },
        (Identifier(s), Bool(b)) => match ram {
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, greater),
            None => Bool(b),
        },
        _ => Identifier("@Those two values are incompatible with the > operator".to_string()),
    }
}

pub fn lesser(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(_)) => Bool(false),
        (Null, Float(_)) => Bool(false),
        (Int(_), Null) => Bool(false),
        (Float(_), Null) => Bool(false),
        (Int(v), Int(v2)) => Bool(v < v2),
        (Int(v), Float(f)) => Bool((v as f64) < f),
        (Float(v), Float(f)) => Bool(v < f),
        (Float(v), Int(i1)) => Bool(v < (i1 as f64)),
        (Rational(_), Null) => Bool(true),
        (Null, Rational(_)) => Bool(true),
        (Rational(s), Rational(s2)) => Bool(s < s2),
        (Rational(s), Int(i)) => Bool(s < Rationals::new(1, i)),
        (Int(i), Rational(s)) => Bool(Rationals::new(1, i) < s),
        (Rational(s), Float(f)) => Bool(s.approx() < f),
        (Float(f), Rational(s)) => Bool(f < s.approx()),
        (Bool(b), Int(_)) => Bool(b),
        (Bool(b), Float(_)) => Bool(b),
        (Int(_), Bool(b)) => Bool(b),
        (Float(_), Bool(b)) => Bool(b),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Identifier(s), Identifier(s2)) => match ram {
            None => Identifier(s),
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, lesser),
        },
        (Identifier(s), Int(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Int(i), ram, lesser),
            None => Int(i),
        },
        (Null, Identifier(s)) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, lesser),
            None => Identifier(s),
        },
        (Rational(s), Identifier(ss)) => match ram {
            Some(_) => {
                apply_operator_reverse(Rational(s.clone()), Identifier(ss.clone()), ram, lesser)
            }
            None => Rational(s),
        },
        (Identifier(ss), Rational(s)) => match ram {
            Some(_) => apply_operator(Identifier(ss), Rational(s), ram, lesser),
            None => Rational(s),
        },
        (Identifier(s), Null) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, lesser),
            None => Identifier(s),
        },
        (Int(i), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Int(i), Identifier(s), ram, lesser),
            None => Int(i),
        },
        (Identifier(s), Float(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Float(i), ram, lesser),
            None => Float(i),
        },
        (Float(i), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Float(i), Identifier(s), ram, lesser),
            None => Float(i),
        },
        (Bool(b), Identifier(s)) => match ram {
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, lesser),
            None => Bool(b),
        },
        (Identifier(s), Bool(b)) => match ram {
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, lesser),
            None => Bool(b),
        },
        _ => Identifier("@Those two values are incompatible with the < operator".to_string()),
    }
}

pub fn greater_or_equal(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(_)) => Bool(true),
        (Null, Float(_)) => Bool(true),
        (Int(_), Null) => Bool(true),
        (Float(_), Null) => Bool(true),
        (Int(v), Int(v2)) => Bool(v >= v2),
        (Int(v), Float(f)) => Bool((v as f64) >= f),
        (Float(v), Float(f)) => Bool(v >= f),
        (Float(v), Int(i1)) => Bool(v >= (i1 as f64)),
        (Bool(b), Int(_)) => Bool(b),
        (Bool(b), Float(_)) => Bool(b),
        (Int(_), Bool(b)) => Bool(b),
        (Float(_), Bool(b)) => Bool(b),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b == b2),

        (Rational(_), Null) => Bool(true),
        (Null, Rational(_)) => Bool(true),
        (Rational(s), Rational(s2)) => Bool(s >= s2),
        (Rational(s), Int(i)) => Bool(s >= Rationals::new(1, i)),
        (Int(i), Rational(s)) => Bool(Rationals::new(1, i) >= s),
        (Rational(s), Float(f)) => Bool(s.approx() >= f),
        (Float(f), Rational(s)) => Bool(f >= s.approx()),
        (Identifier(s), Identifier(s2)) => match ram {
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, greater_or_equal),
            None => Identifier(s),
        },
        (Identifier(s), Int(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Int(i), ram, greater_or_equal),
            None => Int(i),
        },
        (Null, Identifier(s)) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, greater_or_equal),
            None => Identifier(s),
        },
        (Identifier(s), Null) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, greater_or_equal),
            None => Identifier(s),
        },
        (Rational(s), Identifier(ss)) => match ram {
            Some(_) => apply_operator_reverse(
                Rational(s.clone()),
                Identifier(ss.clone()),
                ram,
                greater_or_equal,
            ),
            None => Rational(s),
        },
        (Identifier(ss), Rational(s)) => match ram {
            None => Rational(s),
            Some(_) => apply_operator(Identifier(ss), Rational(s), ram, greater_or_equal),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Int(i),
            Some(_) => apply_operator_reverse(Int(i), Identifier(s), ram, greater_or_equal),
        },
        (Identifier(s), Float(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Float(i), ram, greater_or_equal),
            None => Float(i),
        },
        (Float(i), Identifier(s)) => match ram {
            None => Float(i),
            Some(_) => apply_operator_reverse(Float(i), Identifier(s), ram, greater_or_equal),
        },
        (Bool(b), Identifier(s)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, greater_or_equal),
        },
        (Identifier(s), Bool(b)) => match ram {
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, greater_or_equal),
            None => Bool(b),
        },
        _ => Identifier("@Those two values are incompatible with the >= operator".to_string()),
    }
}

pub fn lesser_or_equal(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(_)) => Bool(false),
        (Null, Float(_)) => Bool(false),
        (Int(_), Null) => Bool(false),
        (Float(_), Null) => Bool(false),
        (Int(v), Int(v2)) => Bool(v <= v2),
        (Int(v), Float(f)) => Bool((v as f64) <= f),
        (Float(v), Float(f)) => Bool(v <= f),
        (Float(v), Int(i1)) => Bool(v <= (i1 as f64)),
        (Bool(b), Int(_)) => Bool(b),
        (Bool(b), Float(_)) => Bool(b),
        (Int(_), Bool(b)) => Bool(b),
        (Float(_), Bool(b)) => Bool(b),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Bool(b), Bool(b2)) => Bool(b == b2),

        (Rational(_), Null) => Bool(true),
        (Null, Rational(_)) => Bool(true),
        (Rational(s), Rational(s2)) => Bool(s <= s2),
        (Rational(s), Int(i)) => Bool(s <= Rationals::new(1, i)),
        (Int(i), Rational(s)) => Bool(Rationals::new(1, i) <= s),
        (Rational(s), Float(f)) => Bool(s.approx() <= f),
        (Float(f), Rational(s)) => Bool(f <= s.approx()),
        (Identifier(s), Identifier(s2)) => match ram {
            None => Identifier(s),
            Some(_) => apply_operator(Identifier(s), Identifier(s2), ram, lesser_or_equal),
        },
        (Identifier(s), Int(i)) => match ram {
            Some(_) => apply_operator(Identifier(s), Int(i), ram, lesser_or_equal),
            None => Int(i),
        },
        (Rational(s), Identifier(ss)) => match ram {
            Some(_) => apply_operator_reverse(
                Rational(s.clone()),
                Identifier(ss.clone()),
                ram,
                lesser_or_equal,
            ),
            None => Rational(s),
        },
        (Identifier(ss), Rational(s)) => match ram {
            None => Rational(s),
            Some(_) => apply_operator(Identifier(ss), Rational(s), ram, lesser_or_equal),
        },
        (Null, Identifier(s)) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, lesser_or_equal),
            None => Identifier(s),
        },
        (Identifier(s), Null) => match ram {
            Some(_) => apply_operator(Identifier(s), Null, ram, lesser_or_equal),
            None => Identifier(s),
        },
        (Int(i), Identifier(s)) => match ram {
            None => Int(i),
            Some(_) => apply_operator_reverse(Int(i), Identifier(s), ram, lesser_or_equal),
        },
        (Identifier(s), Float(i)) => match ram {
            None => Float(i),
            Some(_) => apply_operator(Identifier(s), Float(i), ram, lesser_or_equal),
        },
        (Float(i), Identifier(s)) => match ram {
            None => Float(i),
            Some(_) => apply_operator_reverse(Float(i), Identifier(s), ram, lesser_or_equal),
        },
        (Bool(b), Identifier(s)) => match ram {
            None => Bool(b),
            Some(_) => apply_operator_reverse(Bool(b), Identifier(s), ram, lesser_or_equal),
        },
        (Identifier(s), Bool(b)) => match ram {
            Some(_) => apply_operator(Identifier(s), Bool(b), ram, lesser_or_equal),
            None => Bool(b),
        },
        _ => Identifier("@Those two values are incompatible with the <= operator".to_string()),
    }
}

pub fn equal(
    i: Parameters,
    i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match (i, i2) {
        (Null, Int(_)) => Bool(true),
        (Null, Float(_)) => Bool(true),
        (Int(_), Null) => Bool(true),
        (Float(_), Null) => Bool(true),
        (Int(v), Int(v2)) => Bool(v == v2),
        (Int(v), Float(f)) => Bool((v as f64) == f),
        (Float(v), Float(f)) => Bool(v == f),
        (Float(v), Int(i1)) => Bool(v == (i1 as f64)),
        (Bool(_), Int(_)) => Bool(false),
        (Bool(_), Float(_)) => Bool(false),
        (Int(_), Bool(_)) => Bool(false),
        (Float(_), Bool(_)) => Bool(false),
        (Bool(_), Null) => Bool(false),
        (Null, Bool(_)) => Bool(false),
        (Bool(b), Bool(b2)) => Bool(b == b2),

        (Rational(_), Null) => Bool(true),
        (Null, Rational(_)) => Bool(true),
        (Rational(s), Rational(s2)) => Bool(s == s2),
        (Rational(s), Int(i)) => Bool(s == Rationals::new(1, i)),
        (Int(i), Rational(s)) => Bool(Rationals::new(1, i) == s),
        (Rational(s), Float(f)) => Bool(s.approx() == f),
        (Float(f), Rational(s)) => Bool(f == s.approx()),
        (Identifier(s), Identifier(s2)) => {
            apply_operator(Identifier(s), Identifier(s2), ram, equal)
        }
        (Identifier(s), Int(i)) => apply_operator(Identifier(s), Int(i), ram, equal),
        (Null, Identifier(s)) => apply_operator(Identifier(s), Null, ram, equal),
        (Identifier(s), Null) => apply_operator(Identifier(s), Null, ram, equal),
        (Int(i), Identifier(s)) => apply_operator_reverse(Int(i), Identifier(s), ram, equal),
        (Identifier(s), Float(i)) => apply_operator(Identifier(s), Float(i), ram, equal),
        (Float(i), Identifier(s)) => apply_operator_reverse(Float(i), Identifier(s), ram, equal),
        (Bool(b), Identifier(s)) => apply_operator_reverse(Bool(b), Identifier(s), ram, equal),
        (Identifier(s), Bool(b)) => apply_operator(Identifier(s), Bool(b), ram, equal),

        (Rational(s), Identifier(ss)) => {
            apply_operator_reverse(Rational(s.clone()), Identifier(ss.clone()), ram, equal)
        }
        (Identifier(ss), Rational(s)) => apply_operator(Identifier(ss), Rational(s), ram, equal),

        _ => Identifier("@Those two values are incompatible with the == operator".to_string()),
    }
}

pub fn not(
    i: Parameters,
    _i2: Parameters,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    match i {
        Bool(b) => Bool(!b),
        Identifier(s) => apply_operator(Identifier(s), Null, ram, not),
        _ => Bool(false),
    }
}

pub fn and(i: Parameters, i2: Parameters, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
    match (i, i2) {
        (Bool(b), Bool(b2)) => Bool(b && b2),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Identifier(s), Bool(b)) => apply_operator(Identifier(s), Bool(b), ram, and),
        (Bool(b), Identifier(s)) => apply_operator_reverse(Bool(b), Identifier(s), ram, and),
        (Identifier(s), Identifier(s2)) => apply_operator(Identifier(s), Identifier(s2), ram, and),
        _ => Bool(false),
    }
}

pub fn or(i: Parameters, i2: Parameters, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
    match (i, i2) {
        (Bool(b), Bool(b2)) => Bool(b || b2),
        (Bool(b), Null) => Bool(b),
        (Null, Bool(b)) => Bool(b),
        (Identifier(s), Bool(b)) => apply_operator(Identifier(s), Bool(b), ram, or),
        (Bool(b), Identifier(s)) => apply_operator_reverse(Bool(b), Identifier(s), ram, or),
        (Identifier(s), Identifier(s2)) => apply_operator(Identifier(s), Identifier(s2), ram, or),
        _ => Bool(false),
    }
}

#[cfg(test)]
mod test {
    use crate::functions::add::add;
    use crate::functions::divide::divide;
    use crate::functions::minus::minus;
    use crate::functions::mult::mult;
    use crate::parsing::ast::Parameters;

    #[test]
    pub fn test_add_null() {
        let expected = Int(1);
        let result = add(Int(1), Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_simple() {
        let expected = Int(2);
        let result = add(Int(1), Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_float() {
        let expected = Float(2.1);
        let result = add(Float(0.1), Float(2.0), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_int_float() {
        let expected = Float(2.1);
        let result = add(Int(2), Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_add_float_int() {
        let expected = Float(2.1);
        let result = add(Float(0.1), Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_null() {
        let expected = Int(-1);
        let result = minus(Int(1), Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_null_rev() {
        let expected = Int(-1);
        let result = minus(Null, Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_simple() {
        let expected = Int(0);
        let result = minus(Int(1), Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_float() {
        let expected = Float(1.9);
        let result = minus(Float(2.0), Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_int_float() {
        let expected = Float(1.9);
        let result = minus(Int(2), Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_minus_float_int() {
        let expected = Float(-1.9);
        let result = minus(Float(0.1), Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_null() {
        let expected = Int(1);
        let result = mult(Int(1), Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_simple() {
        let expected = Int(2);
        let result = mult(Int(1), Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_float() {
        let expected = Float(0.2);
        let result = mult(Float(0.1), Float(2.0), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_int_float() {
        let expected = Float(0.2);
        let result = mult(Int(2), Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_mult_float_int() {
        let expected = Float(0.2);
        let result = mult(Float(0.1), Int(2), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_null() {
        let expected = Int(1);
        let result = divide(Int(1), Null, None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_simple() {
        let expected = Rational(crate::exact_math::rationals::Rationals { under: 1, over: 1 });
        let result = divide(Int(1), Int(1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_float() {
        let expected = Float(0.05);
        let result = divide(Float(0.1), Float(2.0), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_int_float() {
        let expected = Float(20.0);
        let result = divide(Int(2), Float(0.1), None);
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_divide_float_int() {
        let expected = Float(0.05);
        let result = divide(Float(0.1), Int(2), None);
        assert_eq!(result, expected);
    }
}
