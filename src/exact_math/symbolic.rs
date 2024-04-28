use crate::parsing::ast::Ast;
use crate::parsing::ast::Ast::*;
use crate::parsing::ast::Parameters::*;

fn simplify(ast: &Ast) -> Ast {
    let null_vector = (&Null, &Box::from(Nil), &Box::from(Nil));
    let (value, left, right) = match ast {
        Ast::Node { value, left, right } => (value, left, right),
        _ => null_vector,
    };

    match value {
        Null | Int(_) | Str(_) | Float(_) | Bool(_) | Identifier(_) | Rational(_) => {
            return ast.clone()
        }
        PlusOperation => {}
        _ => (),
    };

    ast.clone()
}
