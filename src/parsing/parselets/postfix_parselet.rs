use crate::{
    lexing::token::Token,
    parsing::{ast::Ast, parser::CalcParser},
};

pub trait PostfixParselet {
    fn parse(&self, parser: &mut CalcParser, lhs: &Ast, token: &Token) -> Ast;
}

pub struct IgnoreParselet {}

impl PostfixParselet for IgnoreParselet {
    fn parse(&self, _parser: &mut CalcParser, lhs: &Ast, _token: &Token) -> Ast {
        Ast::Ignore {
            left: lhs.clone().into(),
            right: Ast::Nil.into(),
        }
    }
}
