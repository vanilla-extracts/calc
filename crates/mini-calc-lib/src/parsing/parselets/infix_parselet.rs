use crate::lexing::token::{Precedence, Token, TokenType};
use crate::parsing::ast::Ast::Call;
use crate::parsing::ast::{token_to_parameter, Ast, Parameters};
use crate::parsing::parser::CalcParser;

/// # Infix Parselet
/// Trait defining a parselet for an infix operator / expression
/// Infix is for example the '+' operator
/// Function parse:
///  Takes a reference to itself
///  Takes a mutable reference to a parser
///  Takes a reference to the lhs expression
///  Takes a reference to the current token
///  Returns an Ast representing the operator
/// Function get_precendence:
///  Takes a reference to itself
///  Returns the precedence of the operator as a sixty four bits integer
pub trait InfixParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, token: &Token) -> Ast;
    fn get_precedence(&self) -> i64;
}

/// # Assign
/// Parselet for the assign operator (var=value)
#[derive(Clone)]
pub struct AssignParselet {}

/// # Call
/// Parselet for the call operation (f(x))
pub struct CallParselet {}

/// # Ignore
/// Parselet for the ignore parselet (lhs; rhs)
pub struct IgnoreParselet {}

/// # Operator
/// Parselet for all operators (+,*,/,etc.)
pub struct OperatorInfixParselet {
    pub is_right: bool,
    pub precedence: i64,
}

impl InfixParselet for IgnoreParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, _token: &Token) -> Ast {
        let right = parser.parse_expression(self.get_precedence());
        Ast::Ignore {
            left: left.clone().into(),
            right: right.into(),
        }
    }
    fn get_precedence(&self) -> i64 {
        Precedence::Ignore as i64
    }
}

impl InfixParselet for OperatorInfixParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, token: &Token) -> Ast {
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
    fn parse(&self, parser: &mut CalcParser, left: &Ast, _token: &Token) -> Ast {
        let right = parser.parse_expression(self.get_precedence());
        Ast::Node {
            value: Parameters::Assign,
            left: Box::new(left.clone()),
            right: Box::new(right),
        }
    }

    fn get_precedence(&self) -> i64 {
        Precedence::Assignment as i64
    }
}

impl InfixParselet for CallParselet {
    fn parse(&self, parser: &mut CalcParser, left: &Ast, _token: &Token) -> Ast {
        let name = match left {
            Ast::Nil => "",
            Ast::Node {
                value: Parameters::Identifier(s),
                left: _left,
                right: _right,
            } => s.as_str(),
            _ => "",
        };

        let mut lst: Vec<Ast> = Vec::new();
        if !parser.match_token(TokenType::Rpar) {
            lst.push(parser.parse_expression_empty());
            while parser.match_token(TokenType::Comma) {
                parser.consume();
                let ast = parser.parse_expression_empty();
                lst.push(ast);
            }
            parser.consume_expected(TokenType::Rpar);
        }
        Call {
            name: name.to_string(),
            lst,
        }
    }

    fn get_precedence(&self) -> i64 {
        Precedence::Call as i64
    }
}
