use crate::lexing::token::{Precedence, Token, TokenType};
use crate::parsing::ast::Ast::Call;
use crate::parsing::ast::{token_to_parameter, Ast, Parameters};
use crate::parsing::parser::CalcParser;

pub trait InfixParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, token: Token) -> Ast;
    fn get_precedence(&self) -> i64;
}

#[derive(Clone)]
pub struct AssignParselet {}

pub struct CallParselet {}

pub struct NullParset {}

pub struct IgnorePostfixParselet {}

pub struct OperatorInfixParselet {
    pub is_right: bool,
    pub precedence: i64,
}

impl InfixParselet for IgnorePostfixParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, _token: Token) -> Ast {
        let right = parser.parse_expression(self.get_precedence());
        Ast::Ignore {
            left: left.clone().into(),
            right: right.into(),
        }
    }
    fn get_precedence(&self) -> i64 {
        Precedence::IGNORE as i64
    }
}

impl InfixParselet for OperatorInfixParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, token: Token) -> Ast {
        let right = parser.parse_expression(if self.is_right {
            self.get_precedence() - 1
        } else {
            self.get_precedence()
        });
        let param = token_to_parameter(token);
        Ast::Node {
            value: param,
            left: Box::new(left.clone()),
            right: Box::new(right),
        }
    }

    fn get_precedence(&self) -> i64 {
        self.precedence
    }
}

impl InfixParselet for AssignParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, _token: Token) -> Ast {
        let right = parser.parse_expression_empty();
        Ast::Node {
            value: Parameters::Assign,
            left: Box::new(left.clone()),
            right: Box::new(right),
        }
    }

    fn get_precedence(&self) -> i64 {
        Precedence::ASSIGNMENT as i64
    }
}

impl InfixParselet for CallParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, _token: Token) -> Ast {
        let name = match left {
            Ast::Nil => "",
            Ast::Node {
                value: v,
                left: _left,
                right: _right,
            } => match v {
                Parameters::Identifier(s) => s.as_str(),
                _ => "",
            },
            _ => "",
        };

        let mut lst: Vec<Ast> = Vec::new();
        if !parser.match_token(TokenType::RPAR) {
            lst.push(parser.parse_expression_empty());
            while parser.match_token(TokenType::COMMA) {
                parser.consume();
                let ast = parser.parse_expression_empty();
                lst.push(ast);
            }
            parser.consume_expected(TokenType::RPAR);
        }
        Call {
            name: name.to_string(),
            lst,
        }
    }

    fn get_precedence(&self) -> i64 {
        Precedence::CALL as i64
    }
}

impl InfixParselet for NullParset {
    fn parse(&self, _parser: &mut CalcParser, left: &Ast, _token: Token) -> Ast {
        left.clone()
    }

    fn get_precedence(&self) -> i64 {
        0
    }
}
