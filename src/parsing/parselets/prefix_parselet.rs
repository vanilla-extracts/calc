use crate::lexing::token::{Precedence, Token, TokenType};
use crate::parsing::ast::{token_to_parameter, Ast};
use crate::parsing::parser::CalcParser;

pub trait PrefixParselet {
    fn parse(&self, parser: &mut CalcParser, token: &Token) -> Ast;
}

#[derive(Clone)]
pub struct ValueParselet {}

#[derive(Clone)]
pub struct OperatorPrefixParselet {}

#[derive(Clone)]
pub struct GroupParselet {}

#[derive(Clone)]
pub struct ScopeParselet {}

#[derive(Clone)]
pub struct VecParselet {}

#[derive(Clone)]
pub struct QuoteParselet {}

#[derive(Clone)]
pub struct IfThenElseParselet {
    pub precedence: i64,
}

#[derive(Clone)]
pub struct WhileParselet {}

impl PrefixParselet for ValueParselet {
    fn parse(&self, _parser: &mut CalcParser, token: &Token) -> Ast {
        Ast::Node {
            value: token_to_parameter(token),
            left: Box::from(Ast::Nil),
            right: Box::from(Ast::Nil),
        }
    }
}

impl PrefixParselet for OperatorPrefixParselet {
    fn parse(&self, parser: &mut CalcParser, token: &Token) -> Ast {
        let operand = parser.parse_expression(Precedence::Prefix as i64);
        Ast::Node {
            value: token_to_parameter(token),
            left: Box::from(operand),
            right: Box::from(Ast::Nil),
        }
    }
}

impl PrefixParselet for GroupParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let expression = parser.parse_expression_empty();
        parser.consume_expected(TokenType::Rpar);
        expression
    }
}

impl PrefixParselet for ScopeParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let expression = parser.parse_expression_empty();
        parser.consume_expected(TokenType::Rsb);
        expression
    }
}

impl PrefixParselet for VecParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let mut vec: Vec<Ast> = Vec::new();

        if !parser.match_token(TokenType::Rbracket) {
            vec.push(parser.parse_expression_empty());
            while parser.match_token(TokenType::Comma) {
                parser.consume();
                vec.push(parser.parse_expression_empty());
            }
            parser.consume_expected(TokenType::Rbracket);
        }

        Ast::Node {
            value: crate::parsing::ast::Parameters::Vector(Box::from(vec)),
            left: Box::new(Ast::Nil),
            right: Box::new(Ast::Nil),
        }
    }
}

impl PrefixParselet for QuoteParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let mut str: String = String::new();

        if !parser.match_token(TokenType::Quote) {
            while !parser.match_token(TokenType::Quote) {
                match parser.consume() {
                    Token::Identifier(s) => str = str + &s.to_string(),

                    t => str = str + &t.to_string(),
                }
            }
            parser.consume_expected(TokenType::Quote);
        }

        Ast::Node {
            value: crate::parsing::ast::Parameters::Str(str.to_string()),
            left: Box::new(Ast::Nil),
            right: Box::new(Ast::Nil),
        }
    }
}

impl PrefixParselet for IfThenElseParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let cond_expr = parser.parse_expression_empty();
        parser.consume_expected(TokenType::Then);
        let lhs = parser.parse_expression(self.precedence);
        parser.consume_expected(TokenType::Else);
        let rhs = parser.parse_expression(self.precedence);

        Ast::Conditional {
            condition: cond_expr.into(),
            then_branch: lhs.into(),
            else_branch: rhs.into(),
        }
    }
}

impl PrefixParselet for WhileParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let cond_expr = parser.parse_expression(Precedence::While as i64);
        parser.consume_expected(TokenType::Do);
        let body = parser.parse_expression(Precedence::While as i64);

        Ast::While {
            condition: cond_expr.into(),
            body: body.into(),
        }
    }
}
