use crate::parsing::ast::Ast::*;
use crate::parsing::ast::Parameters::*;
use crate::parsing::ast::{Ast, Parameters};

impl Ast {
    fn simplify(&self) -> &Ast {
        let null_vector = (&Null, &Box::from(Nil), &Box::from(Nil));
        let (value, left, right) = match &self {
            Ast::Node { value, left, right } => (value, left, right),
            _ => null_vector,
        };

        match value {
            Null | Int(_) | Str(_) | Float(_) | Bool(_) | Identifier(_) | Rational(_) => {
                return self
            }
            PlusOperation => {}
            _ => (),
        };

        self
    }
}
