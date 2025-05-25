use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    PLUS,
    MINUS,
    MULTIPLICATION,
    DIVIDE,
    EXPO,
    EQUALITY,
    GreaterThan,
    LesserThan,
    GreaterOrEqual,
    LesserOrEqual,
    And,
    Or,
    NOT,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    OPE(Operator),
    IDENTIFIER(String),
    INT(i64),
    FLOAT(f64),
    BOOL(bool),
    EQUAL,
    RPAR,
    LPAR,
    RBRACKET,
    LBRACKET,
    COMMA,
    Null,
    QUOTE,
    WHITESPACE,
    PreAnd,
    PreOr,
    IF,
    THEN,
    ELSE,
    WHILE,
    DO,
    IGNORE,
    RSB,
    LSB,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TokenType {
    PLUS,
    MINUS,
    MULTIPLICATION,
    DIVIDE,
    IDENTIFIER,
    INT,
    FLOAT,
    EQUAL,
    EQUALITY,
    GREATER,
    LESSER,
    GREATEREQ,
    OR,
    AND,
    LESSEREQ,
    NOT,
    BOOL,
    RPAR,
    LPAR,
    RBRACKET,
    LBRACKET,
    Null,
    COMMA,
    WHITESPACE,
    EXPO,
    QUOTE,
    IF,
    THEN,
    ELSE,
    WHILE,
    DO,
    IGNORE,
    RSB,
    LSB,
}

pub enum Precedence {
    ASSIGNMENT = 10,
    CONDITIONAL = 20,
    SUM = 40,
    MINUS = 30,
    PRODUCT = 50,
    DIVIDE = 45,
    EXPONENT = 60,
    IGNORE = 70,
    CALL = 100,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::PLUS => write!(f, "+"),
            Operator::MINUS => write!(f, "-"),
            Operator::DIVIDE => write!(f, "/"),
            Operator::MULTIPLICATION => write!(f, "*"),
            Operator::EXPO => write!(f, "^"),
            Operator::EQUALITY => write!(f, "=="),
            Operator::GreaterOrEqual => write!(f, ">="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::LesserOrEqual => write!(f, "<="),
            Operator::LesserThan => write!(f, "<"),
            Operator::NOT => write!(f, "!"),
            Operator::Or => write!(f, "||"),
            Operator::And => write!(f, "&&"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LPAR => write!(f, "("),
            Token::RPAR => write!(f, ")"),
            Token::EQUAL => write!(f, "="),
            Token::FLOAT(i) => write!(f, "{}", i),
            Token::INT(i) => write!(f, "{}", i),
            Token::IDENTIFIER(s) => write!(f, "{}", s),
            Token::OPE(s) => write!(f, "{}", s),
            Token::COMMA => write!(f, ","),
            Token::Null => write!(f, "Null"),
            Token::BOOL(b) => write!(f, "{b}"),
            Token::PreAnd => write!(f, ""),
            Token::PreOr => write!(f, ""),
            Token::RBRACKET => write!(f, "]"),
            Token::LBRACKET => write!(f, "["),
            Token::QUOTE => write!(f, "\""),
            Token::WHITESPACE => write!(f, " "),
            Token::IF => write!(f, "if"),
            Token::THEN => write!(f, "then"),
            Token::ELSE => write!(f, "else"),
            Token::WHILE => write!(f, "while"),
            Token::DO => write!(f, "do"),
            Token::IGNORE => write!(f, "!"),
            Token::RSB => write!(f, "{{"),
            Token::LSB => write!(f, "}}"),
        }
    }
}

impl Token {
    pub fn to_token_type(&self) -> TokenType {
        match &self {
            Token::OPE(p) => match p {
                Operator::PLUS => TokenType::PLUS,
                Operator::MINUS => TokenType::MINUS,
                Operator::MULTIPLICATION => TokenType::MULTIPLICATION,
                Operator::DIVIDE => TokenType::DIVIDE,
                Operator::EXPO => TokenType::EXPO,
                Operator::EQUALITY => TokenType::EQUALITY,
                Operator::GreaterThan => TokenType::GREATER,
                Operator::GreaterOrEqual => TokenType::GREATEREQ,
                Operator::LesserThan => TokenType::LESSER,
                Operator::LesserOrEqual => TokenType::LESSEREQ,
                Operator::NOT => TokenType::NOT,
                Operator::And => TokenType::AND,
                Operator::Or => TokenType::OR,
            },
            Token::IDENTIFIER(_) => TokenType::IDENTIFIER,
            Token::INT(_) => TokenType::INT,
            Token::FLOAT(_) => TokenType::FLOAT,
            Token::EQUAL => TokenType::EQUAL,
            Token::RPAR => TokenType::RPAR,
            Token::LPAR => TokenType::LPAR,
            Token::COMMA => TokenType::COMMA,
            Token::Null => TokenType::Null,
            Token::BOOL(_) => TokenType::BOOL,
            Token::LBRACKET => TokenType::LBRACKET,
            Token::RBRACKET => TokenType::RBRACKET,
            Token::QUOTE => TokenType::QUOTE,
            Token::WHITESPACE => TokenType::WHITESPACE,
            Token::IF => TokenType::IF,
            Token::ELSE => TokenType::ELSE,
            Token::THEN => TokenType::THEN,
            Token::WHILE => TokenType::WHILE,
            Token::DO => TokenType::DO,
            Token::IGNORE => TokenType::IGNORE,
            Token::LSB => TokenType::LSB,
            Token::RSB => TokenType::RSB,
            _ => TokenType::Null,
        }
    }
}
#[cfg(test)]
mod test {
    use super::{Token, TokenType};

    #[test]
    fn test_token_type_operators_plus() {
        let expected = TokenType::PLUS;
        let value = Token::OPE(super::Operator::PLUS).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_minus() {
        let expected = TokenType::MINUS;
        let value = Token::OPE(super::Operator::MINUS).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_mult() {
        let expected = TokenType::MULTIPLICATION;
        let value = Token::OPE(super::Operator::MULTIPLICATION).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_divide() {
        let expected = TokenType::DIVIDE;
        let value = Token::OPE(super::Operator::DIVIDE).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_expo() {
        let expected = TokenType::EXPO;
        let value = Token::OPE(super::Operator::EXPO).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_equality() {
        let expected = TokenType::EQUALITY;
        let value = Token::OPE(super::Operator::EQUALITY).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_greater() {
        let expected = TokenType::GREATER;
        let value = Token::OPE(super::Operator::GreaterThan).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_lesser() {
        let expected = TokenType::LESSER;
        let value = Token::OPE(super::Operator::LesserThan).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_greaterq() {
        let expected = TokenType::GREATEREQ;
        let value = Token::OPE(super::Operator::GreaterOrEqual).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_lesserq() {
        let expected = TokenType::LESSEREQ;
        let value = Token::OPE(super::Operator::LesserOrEqual).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_and() {
        let expected = TokenType::AND;
        let value = Token::OPE(super::Operator::And).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_or() {
        let expected = TokenType::OR;
        let value = Token::OPE(super::Operator::Or).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_not() {
        let expected = TokenType::NOT;
        let value = Token::OPE(super::Operator::NOT).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_identifier() {
        let expected = TokenType::IDENTIFIER;
        let value = Token::IDENTIFIER("s".to_string()).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_int() {
        let expected = TokenType::INT;
        let value = Token::INT(0).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_float() {
        let expected = TokenType::FLOAT;
        let value = Token::FLOAT(0.0).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_equal() {
        let expected = TokenType::EQUAL;
        let value = Token::EQUAL.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_lpar() {
        let expected = TokenType::LPAR;
        let value = Token::LPAR.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_rpar() {
        let expected = TokenType::RPAR;
        let value = Token::RPAR.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_bool() {
        let expected = TokenType::BOOL;
        let value = Token::BOOL(false).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_null() {
        let expected = TokenType::Null;
        let value = Token::Null.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_comma() {
        let expected = TokenType::COMMA;
        let value = Token::COMMA.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_rbracket() {
        let expected = TokenType::RBRACKET;
        let value = Token::RBRACKET.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_lbracket() {
        let expected = TokenType::LBRACKET;
        let value = Token::LBRACKET.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_whitespace() {
        let expected = TokenType::WHITESPACE;
        let value = Token::WHITESPACE.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_quote() {
        let expected = TokenType::QUOTE;
        let value = Token::QUOTE.to_token_type();
        assert_eq!(value, expected);
    }
}
