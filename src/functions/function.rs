use std::collections::HashMap;

use crate::exact_math::rationals::Rationals;
use crate::exact_math::symbolic::size;
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
        Parameters::Identifier(ref s) => s,
        _ => "",
    };
    if s == "" {
        return Parameters::Null;
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
