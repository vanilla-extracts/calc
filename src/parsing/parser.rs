use core::slice::Iter;

use crate::lexing::token::Token::*;
use crate::lexing::token::{Precedence, Token, TokenType};
use crate::parsing::ast::Ast;
use crate::parsing::parselets::infix_parselet::{
    AssignParselet, CallParselet, InfixParselet, OperatorInfixParselet,
};
use crate::parsing::parselets::prefix_parselet::{
    GroupParselet, OperatorPrefixParselet, PrefixParselet, ValueParselet,
};

use super::parselets::postfix_parselet::{IgnoreParselet, PostfixParselet};
use super::parselets::prefix_parselet::{
    IfThenElseParselet, QuoteParselet, ScopeParselet, VecParselet, WhileParselet,
};

#[derive(Clone)]
pub struct CalcParser<'a> {
    tokens: Iter<'a, Token>,
    read: Vec<Token>,
}

pub fn init_calc_parser(input: &Vec<Token>) -> CalcParser {
    CalcParser {
        tokens: input.iter(),
        read: Vec::new(),
    }
}

impl CalcParser<'_> {
    pub fn parse(&mut self) -> Ast {
        self.parse_expression_empty()
    }
    pub fn parse_expression(&mut self, precedence: i64) -> Ast {
        let mut token = self.consume();
        let prefix = self.get_prefix_parselet(&token.to_token_type());

        let mut left = match prefix {
            Some(p) => p.parse(self, &token),
            None => Ast::Nil,
        };

        left = match self.get_postfix_parselet(&token.to_token_type()) {
            None => left,
            Some(p) => p.parse(self, &left, &token),
        };

        while precedence < self.get_precedence() {
            token = self.consume();
            left = match self.get_infix_parselet(&token.to_token_type()) {
                Some(p) => p.parse(self, &left, &token),
                None => left,
            };
        }
        match self.get_postfix_parselet(&token.to_token_type()) {
            None => left,
            Some(p) => p.parse(self, &left, &token),
        }
    }

    pub fn parse_expression_empty(&mut self) -> Ast {
        self.parse_expression(0)
    }
    fn look_ahead(&mut self, distance: usize) -> Token {
        while distance >= self.read.len() {
            match self.tokens.next() {
                None => break,
                Some(t) => self.read.push(t.clone()),
            }
        }
        match self.read.get(distance) {
            None => Null,
            Some(t) => t.clone(),
        }
    }
    pub fn consume(&mut self) -> Token {
        self.look_ahead(0);
        if self.read.len() == 0 {
            return Null;
        }
        self.read.remove(0)
    }

    pub fn match_token(&mut self, expected: TokenType) -> bool {
        let token = self.look_ahead(0);
        if token.to_token_type() != expected {
            return false;
        }
        return true;
    }

    pub fn consume_expected(&mut self, expected: TokenType) -> Token {
        self.look_ahead(0);
        if self.read.len() == 0 {
            return Null;
        }
        match self.read.remove(0) {
            t => {
                if t.to_token_type() == expected {
                    t
                } else {
                    println!("error!");
                    Null
                }
            }
        }
    }

    fn get_precedence(&mut self) -> i64 {
        let token_type = self.look_ahead(0).to_token_type();
        match self.get_infix_parselet(&token_type) {
            Some(t) => t.get_precedence(),
            None => match self.get_postfix_parselet(&token_type) {
                Some(_) => Precedence::POSTFIX as i64,
                None => 0,
            },
        }
    }

    pub fn get_infix_parselet(&self, token_type: &TokenType) -> Option<Box<dyn InfixParselet>> {
        match token_type {
            TokenType::PLUS => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::SUM as i64),
            })),
            TokenType::MINUS => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::MINUS as i64),
            })),
            TokenType::MULTIPLICATION => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::PRODUCT as i64),
            })),
            TokenType::DIVIDE => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::DIVIDE as i64),
            })),
            TokenType::EQUAL => Some(Box::from(AssignParselet {})),
            TokenType::EXPO => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::EXPONENT as i64),
            })),
            TokenType::LPAR => Some(Box::from(CallParselet {})),
            TokenType::NOT => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::EQUALITY => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::LESSER => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::LESSEREQ => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::GREATER => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::GREATEREQ => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::OR => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            TokenType::AND => Some(Box::from(OperatorInfixParselet {
                is_right: false,
                precedence: (Precedence::CONDITIONAL as i64),
            })),
            _ => None,
        }
    }

    pub fn get_prefix_parselet(&self, token_type: &TokenType) -> Option<Box<dyn PrefixParselet>> {
        match token_type {
            TokenType::PLUS => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::MINUS => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::MULTIPLICATION => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::DIVIDE => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::IDENTIFIER => Some(Box::from(ValueParselet {})),
            TokenType::INT => Some(Box::from(ValueParselet {})),
            TokenType::FLOAT => Some(Box::from(ValueParselet {})),
            TokenType::BOOL => Some(Box::from(ValueParselet {})),
            TokenType::LPAR => Some(Box::from(GroupParselet {})),
            TokenType::NOT => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::EQUALITY => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::LESSER => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::LESSEREQ => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::GREATER => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::GREATEREQ => Some(Box::from(OperatorPrefixParselet {})),
            TokenType::LBRACKET => Some(Box::from(VecParselet {})),
            TokenType::QUOTE => Some(Box::from(QuoteParselet {})),
            TokenType::IF => Some(Box::from(IfThenElseParselet {})),
            TokenType::WHILE => Some(Box::from(WhileParselet {})),
            TokenType::LSB => Some(Box::from(ScopeParselet {})),
            _ => None,
        }
    }

    pub fn get_postfix_parselet(&self, token_type: &TokenType) -> Option<Box<dyn PostfixParselet>> {
        match token_type {
            TokenType::IGNORE => Some(Box::from(IgnoreParselet {})),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexing::lexer::lex;
    use crate::parsing::ast::Parameters::{
        DivideOperation, MultiplicationOperation, PlusOperation,
    };
    use crate::parsing::ast::{Ast, Parameters};
    use crate::parsing::parser::{init_calc_parser, CalcParser};

    #[test]
    pub fn test_parse_nil() {
        let b = lex("".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Nil;
        let result = parser.parse();
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_parse_one_token() {
        let b = lex("2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::Int(2),
            left: Box::new(Ast::Nil),
            right: Box::new(Ast::Nil),
        };

        let result = parser.parse();
        assert_eq!(result, expected);
    }

    #[test]
    pub fn test_parse_plus_operation() {
        let b = lex("2+2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::new(Ast::new(Parameters::Int(2))),
            right: Box::new(Ast::new(Parameters::Int(2))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_parse_plus_operation_hard() {
        let b = lex("1+1+1".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::from(Ast::Node {
                value: Parameters::PlusOperation,
                left: Box::from(Ast::new(Parameters::Int(1))),
                right: Box::from(Ast::new(Parameters::Int(1))),
            }),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_parse_minus_operation() {
        let b = lex("2-2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::MinusOperation,
            left: Box::new(Ast::new(Parameters::Int(2))),
            right: Box::new(Ast::new(Parameters::Int(2))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_parse_mult_operation() {
        let b = lex("2*2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::MultiplicationOperation,
            left: Box::new(Ast::new(Parameters::Int(2))),
            right: Box::new(Ast::new(Parameters::Int(2))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_parse_mult_divide_operation() {
        let b = lex("2*2/2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::DivideOperation,
            left: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::new(Ast::new(Parameters::Int(2))),
                right: Box::new(Ast::new(Parameters::Int(2))),
            }),
            right: Box::from(Ast::new(Parameters::Int(2))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_parse_hard_mult_operation() {
        let b = lex("2*2*2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::MultiplicationOperation,
            left: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::from(Ast::new(Parameters::Int(2))),
                right: Box::from(Ast::new(Parameters::Int(2))),
            }),
            right: Box::from(Ast::new(Parameters::Int(2))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_parse_divide_operation() {
        let b = lex("2/2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::DivideOperation,
            left: Box::new(Ast::new(Parameters::Int(2))),
            right: Box::new(Ast::new(Parameters::Int(2))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_or_operation() {
        let b = lex("false || true".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::OrOperation,
            left: Box::new(Ast::new(Parameters::Bool(false))),
            right: Box::new(Ast::new(Parameters::Bool(true))),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_assignment() {
        let b = lex("i=1".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::Assign,
            left: Box::new(Ast::new(Parameters::Identifier("i".to_string()))),
            right: Box::new(Ast::new(Parameters::Int(1))),
        };
        let result = parser.parse();
        assert_eq!(result, expected);
    }

    #[test]
    pub fn simple_parenthesis() {
        let b = lex("1+(1*1)".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::new(Ast::new(Parameters::Int(1))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn hard_parenthesis() {
        let b = lex("1+(1*(1/1))".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            right: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::from(Ast::new(Parameters::Int(1))),
                right: Box::from(Ast::Node {
                    value: Parameters::DivideOperation,
                    left: Box::from(Ast::new(Parameters::Int(1))),
                    right: Box::from(Ast::new(Parameters::Int(1))),
                }),
            }),
            left: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn without_parenthesis() {
        let b = lex("1+1*1".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::new(Ast::new(Parameters::Int(1))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn hard_without_parenthesis() {
        let b = lex("1+1*1/1".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        //1+((1*1)/1)
        let expected = Ast::Node {
            value: PlusOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::Node {
                value: DivideOperation,
                left: Box::from(Ast::Node {
                    value: MultiplicationOperation,
                    left: Box::from(Ast::new(Parameters::Int(1))),
                    right: Box::from(Ast::new(Parameters::Int(1))),
                }),
                right: Box::from(Ast::new(Parameters::Int(1))),
            }),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_left_priority() {
        let b = lex("1+2*2".to_string());
        let parser: &mut CalcParser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::from(Ast::new(Parameters::Int(2))),
                right: Box::from(Ast::new(Parameters::Int(2))),
            }),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_right_priority() {
        let b = lex("2*2+1".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::from(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::from(Ast::new(Parameters::Int(2))),
                right: Box::from(Ast::new(Parameters::Int(2))),
            }),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = parser.parse();
        assert_eq!(result, expected)
    }

    #[test]
    pub fn test_vector_parsing() {
        let b = lex("[2,2,2,2]".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::Vector(Box::new(vec![Ast::new(Parameters::Int(2)); 4])),
            left: Box::new(Ast::Nil),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_string_parsing() {
        let b = lex("\"test 1 2 1 2\"".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Node {
            value: Parameters::Str("test 1 2 1 2".to_string()),
            left: Box::new(Ast::Nil),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_plus() {
        let b = lex("1+1;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::PlusOperation,
                left: Box::new(Ast::new(Parameters::Int(1))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_minus() {
        let b = lex("1-1;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::MinusOperation,
                left: Box::new(Ast::new(Parameters::Int(1))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_mul() {
        let b = lex("1*1;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::MultiplicationOperation,
                left: Box::new(Ast::new(Parameters::Int(1))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_div() {
        let b = lex("1/1;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::DivideOperation,
                left: Box::new(Ast::new(Parameters::Int(1))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_sup() {
        let b = lex("x>5;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::GreaterOperation,
                left: Box::new(Ast::new(Parameters::Identifier("x".to_string()))),
                right: Box::new(Ast::new(Parameters::Int(5))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_less() {
        let b = lex("x<5;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::LesserOperation,
                left: Box::new(Ast::new(Parameters::Identifier("x".to_string()))),
                right: Box::new(Ast::new(Parameters::Int(5))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }
    #[test]
    pub fn test_ignore_parsing_sup_or_equal() {
        let b = lex("x>=5;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::GreaterOrEqualOperation,
                left: Box::new(Ast::new(Parameters::Identifier("x".to_string()))),
                right: Box::new(Ast::new(Parameters::Int(5))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_less_or_equal() {
        let b = lex("x<=5;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::LesserOrEqualOperation,
                left: Box::new(Ast::new(Parameters::Identifier("x".to_string()))),
                right: Box::new(Ast::new(Parameters::Int(5))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_ignore_parsing_condition() {
        let b = lex("if true then 1 else 2;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Conditional {
                condition: Box::new(Ast::new(Parameters::Bool(true))),
                then_branch: Box::new(Ast::new(Parameters::Int(1))),
                else_branch: Box::new(Ast::new(Parameters::Int(5))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }

    #[test]
    pub fn test_condition_else_ignore() {
        let b = lex("if true then 1 else (2;)".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Conditional {
            condition: Box::new(Ast::new(Parameters::Bool(true))),
            then_branch: Box::new(Ast::new(Parameters::Int(1))),
            else_branch: Box::new(Ast::Ignore {
                left: Box::new(Ast::new(Parameters::Int(2))),
                right: Box::new(Ast::Nil),
            }),
        };
        assert_eq!(parser.parse(), expected);
    }
    #[test]
    pub fn test_ignore_parsing_assignment() {
        let b = lex("i=1;".to_string());
        let parser = &mut init_calc_parser(&b);
        let expected = Ast::Ignore {
            left: Box::new(Ast::Node {
                value: Parameters::Assign,
                left: Box::new(Ast::new(Parameters::Identifier("x".to_string()))),
                right: Box::new(Ast::new(Parameters::Int(1))),
            }),
            right: Box::new(Ast::Nil),
        };
        assert_eq!(parser.parse(), expected);
    }
}
