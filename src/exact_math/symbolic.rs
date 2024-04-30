use crate::parsing::ast::Parameters;
use crate::parsing::ast::Parameters::*;

pub fn size(p: &Parameters) -> i32 {
    match p {
        Null
        | Int(_)
        | Str(_)
        | Float(_)
        | Identifier(_)
        | PlusOperation
        | MinusOperation
        | MultiplicationOperation
        | DivideOperation
        | Assign
        | ExpoOperation
        | GreaterOperation
        | GreaterOrEqualOperation
        | LesserOperation
        | LesserOrEqualOperation
        | Equal
        | Bool(_)
        | Rational(_)
        | OrOperation
        | AndOperation
        | Not
        | Vector(_)
        | InterpreterVector(_) => 0,
        Plus(x, y) => 1 + size(x) + size(y),
        Mul(x, y) => 1 + size(x) + size(y),
        Var(x, _, _) => 1 + size(x),
    }
}

#[cfg(test)]
mod test {
    use crate::{
        exact_math::rationals::Rationals,
        parsing::ast::{
            Ast,
            Parameters::{self, *},
        },
    };

    use super::size;
    #[test]
    fn test_leaf() {
        let v = vec![
            Null,
            Int(1),
            Str("".to_string()),
            Float(1.0),
            Identifier("".to_string()),
            PlusOperation,
            MinusOperation,
            MultiplicationOperation,
            DivideOperation,
            Assign,
            ExpoOperation,
            GreaterOperation,
            GreaterOrEqualOperation,
            LesserOperation,
            LesserOrEqualOperation,
            Equal,
            Bool(false),
            Rational(Rationals::new(10, 10)),
            OrOperation,
            AndOperation,
            Not,
            Vector(Box::from(vec![Ast::Nil])),
            InterpreterVector(Box::from(vec![Null])),
        ];

        let should = vec![
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
        ];

        let _ = v
            .into_iter()
            .map(|f| size(&f))
            .zip(should)
            .for_each(|(x, y)| assert_eq!(x, y));
    }

    #[test]
    fn test_size() {
        let should = 2;
        let tree = Plus(
            Box::from(Plus(
                Box::from(Parameters::Int(1)),
                Box::from(Parameters::Int(2)),
            )),
            Box::from(Parameters::Int(3)),
        );
        let result = size(&tree);
        assert_eq!(result, should);
    }
}
