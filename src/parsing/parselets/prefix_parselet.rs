use crate::lexing::token::{Token, TokenType};
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
pub struct IfThenElseParselet {}

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
        let operand = parser.parse_expression_empty();
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
        parser.consume_expected(TokenType::RPAR);
        expression
    }
}

impl PrefixParselet for ScopeParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let expression = parser.parse_expression_empty();
        parser.consume_expected(TokenType::RSB);
        expression
    }
}

impl PrefixParselet for VecParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let mut vec: Vec<Ast> = Vec::new();

        if !parser.match_token(TokenType::RBRACKET) {
            vec.push(parser.parse_expression_empty());
            while parser.match_token(TokenType::COMMA) {
                parser.consume();
                vec.push(parser.parse_expression_empty());
            }
            parser.consume_expected(TokenType::RBRACKET);
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

        if !parser.match_token(TokenType::QUOTE) {
            while !parser.match_token(TokenType::QUOTE) {
                match parser.consume() {
                    Token::IDENTIFIER(s) => str = str + &s.to_string(),

                    t => str = str + &t.to_string(),
                }
            }
            parser.consume_expected(TokenType::QUOTE);
        }

        Ast::Node {
            value: crate::parsing::ast::Parameters::Str(str.trim().to_string()),
            left: Box::new(Ast::Nil),
            right: Box::new(Ast::Nil),
        }
    }
}

impl PrefixParselet for IfThenElseParselet {
    fn parse(&self, parser: &mut CalcParser, _token: &Token) -> Ast {
        let cond_expr = parser.parse_expression_empty();
        parser.consume_expected(TokenType::THEN);
        let lhs = parser.parse_expression(self.precedence);
        parser.consume_expected(TokenType::ELSE);
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
        let cond_expr = parser.parse_expression_empty();
        parser.consume_expected(TokenType::DO);
        let body = parser.parse_expression_empty();

        Ast::While {
            condition: cond_expr.into(),
            body: body.into(),
        }
    }
}
