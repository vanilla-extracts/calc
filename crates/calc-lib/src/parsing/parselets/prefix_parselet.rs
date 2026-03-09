use crate::lexing::token::{Precedence, Token, TokenType};
use crate::parsing::ast::{token_to_parameter, Ast};
use crate::parsing::parser::CalcParser;

/// # Prefix Parselet
/// Trait for defining a prefix operation parselet
/// A prefix operation can be multiple things, for example:
/// - `x` is a prefix, it's a simple value.
/// - `(x)` is a prefix operator, the parentheses contains `x`.
/// - `{x}` same as the last one
/// - `[x]` same idea
/// - `"x"` same idea
/// - `+x` where `+` can be any operator, very useful for negative numbers
/// - `if cond then stuff else other` is a prefix operator.
/// - `while cond do stuff` is also a prefix operator
/// Function parse:
///   Takes a reference to itself
///   Takes a mutable reference to a parser
///   Takes a reference to the current token
///   Returns the Ast describing the operation
pub trait PrefixParselet {
    fn parse(&self, parser: &mut CalcParser, token: &Token) -> Ast;
}

/// # Value
/// Parselet for a simple value
#[derive(Clone)]
pub struct ValueParselet {}

/// # Operator Prefix
/// Generic Parselet for any operator
#[derive(Clone)]
pub struct OperatorPrefixParselet {}

/// # Group
/// Parselet for `(stuff)`
#[derive(Clone)]
pub struct GroupParselet {}

/// # Scope
/// Parselet for `{scope}`
#[derive(Clone)]
pub struct ScopeParselet {}

/// # Vec
/// Parselet for `[stuff]`
#[derive(Clone)]
pub struct VecParselet {}

/// # Quote
/// Parselet for `"stuff"`
#[derive(Clone)]
pub struct QuoteParselet {}

/// # If Then Else
/// Parselet for the `if cond then stuff else other` operation
/// Precedence: sixty four bits integer for the precedence of the `if` operator
#[derive(Clone)]
pub struct IfThenElseParselet {
    pub precedence: i64,
}

/// # While
/// Parselet for the `while cond do stuff` operation
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
            value: crate::parsing::ast::Parameters::Vector(vec),
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
