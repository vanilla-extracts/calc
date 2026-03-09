use std::fmt::{Display, Formatter};

/// # Operator
/// An operator in Calc is any of the following:
/// - `+` addition
/// - `-` substraction
/// - `*` multiplication
/// - `/` division
/// - `^` exponentiation
/// - `==` equality
/// - `>` greater than
/// - `<` lesser than
/// - `>=` greater of equal
/// - `<=` lesser or equal
/// - `&&` boolean and
/// - `||` boolean or
/// - `!` boolean not
/// - `.` vector selection
/// - `++` vector/string concatenation
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiplication,
    Divide,
    Expo,
    Equality,
    GreaterThan,
    LesserThan,
    GreaterOrEqual,
    LesserOrEqual,
    And,
    Or,
    Not,
    Selection,
    ConcatOperation,
}

/// # Token
/// A token can be:
/// - An operator (with the associated Operator, see above)
/// - An identifier (with the associated string)
/// - An integer (with the associated signed sixty four bits integer)
/// - A float (with the associated sixty four bits double precision floating point number)
/// - A boolean (with the associated boolean)
/// - Equal (`=`)
/// - Rpar (`(`)
/// - Lpar (`)`)
/// - Rbracket (`[`)
/// - Lbracket (`]`)
/// - Comma (`,`)
/// - Quote (`"`)
/// - Whitespace (` `)
/// - PreAnd: temporary token to lex a full And operator (`&`)
/// - PreOr: temporary token to lex a full Or operator (`|`)
/// - If (`if`)
/// - Then (`then`)
/// - Else (`else`)
/// - While (`while`)
/// - Do (`do`)
/// - Ignore (`;`)
/// - Rsb (`{`)
/// - Lsb (`}`)
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ope(Operator),
    Identifier(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Equal,
    Rpar,
    Lpar,
    Rbracket,
    Lbracket,
    Comma,
    Null,
    Quote,
    Whitespace,
    PreAnd,
    PreOr,
    If,
    Then,
    Else,
    While,
    Do,
    Ignore,
    Rsb,
    Lsb,
}

/// # TokenType
/// The type (without associated value) of each token, very useful in parsing
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TokenType {
    Plus,
    Minus,
    Multiplication,
    Divide,
    Identifier,
    Int,
    Float,
    Equal,
    Equality,
    Greater,
    Lesser,
    GreaterEq,
    Or,
    And,
    LesserEq,
    Not,
    Bool,
    Rpar,
    Lpar,
    Rbracket,
    Lbracket,
    Null,
    Comma,
    Whitespace,
    Expo,
    Quote,
    If,
    Then,
    Else,
    While,
    Do,
    Ignore,
    Rsb,
    Lsb,
    Selection,
    Concat,
}

/// # Precedence
/// Precedence table for the operator, the higher the number the "stickier" the parsing
/// For example if you have: 3+3*3
/// It will be parsed as 3+(3*3)
/// Because the precedence of product is 50, while the precedence of addition is 40
/// Warning:
///
/// This approach has one *big* downside:
/// The parser as it is now can't handle two operators with the same precedence
/// Which means this: `3/3*3`
/// Which *should* be `(3/3)*3` in "standard math" (because product and division has the same precedence, so it is read left to right)
/// Is parsed as `3/(3*3)` because in this the precedence of the product is higher than the precedence of division
///
/// FIXME: Allow two equal precedence with parsing left to right
pub enum Precedence {
    Ignore = 5,
    IfThenElse = 6,
    While = 7,
    Prefix = 9,
    Assignment = 10,

    // TODO: maybe rename this
    Conditional = 20,
    Selection = 25,
    Concat = 26,
    Minus = 30,
    Sum = 40,
    Divide = 45,
    Product = 50,
    Exponent = 60,
    Call = 100,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Divide => write!(f, "/"),
            Operator::Multiplication => write!(f, "*"),
            Operator::Expo => write!(f, "^"),
            Operator::Equality => write!(f, "=="),
            Operator::GreaterOrEqual => write!(f, ">="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::LesserOrEqual => write!(f, "<="),
            Operator::LesserThan => write!(f, "<"),
            Operator::Not => write!(f, "!"),
            Operator::Or => write!(f, "||"),
            Operator::And => write!(f, "&&"),
            Operator::Selection => write!(f, "."),
            Operator::ConcatOperation => write!(f, "++"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Lpar => write!(f, "("),
            Token::Rpar => write!(f, ")"),
            Token::Equal => write!(f, "="),
            Token::Float(i) => write!(f, "{}", i),
            Token::Int(i) => write!(f, "{}", i),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Ope(s) => write!(f, "{}", s),
            Token::Comma => write!(f, ","),
            Token::Null => write!(f, "Null"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::PreAnd => write!(f, ""),
            Token::PreOr => write!(f, ""),
            Token::Rbracket => write!(f, "]"),
            Token::Lbracket => write!(f, "["),
            Token::Quote => write!(f, "\""),
            Token::Whitespace => write!(f, " "),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::Ignore => write!(f, "!"),
            Token::Rsb => write!(f, "{{"),
            Token::Lsb => write!(f, "}}"),
        }
    }
}

impl Token {
    /// # ToTokenType
    /// Transforms a token into its type
    /// Takes a reference to itself
    /// Returns the equivalent TokenType
    pub fn to_token_type(&self) -> TokenType {
        match &self {
            Token::Ope(p) => match p {
                Operator::Plus => TokenType::Plus,
                Operator::Minus => TokenType::Minus,
                Operator::Multiplication => TokenType::Multiplication,
                Operator::Divide => TokenType::Divide,
                Operator::Expo => TokenType::Expo,
                Operator::Equality => TokenType::Equality,
                Operator::GreaterThan => TokenType::Greater,
                Operator::GreaterOrEqual => TokenType::GreaterEq,
                Operator::LesserThan => TokenType::Lesser,
                Operator::LesserOrEqual => TokenType::LesserEq,
                Operator::Not => TokenType::Not,
                Operator::And => TokenType::And,
                Operator::Or => TokenType::Or,
                Operator::Selection => TokenType::Selection,
                Operator::ConcatOperation => TokenType::Concat,
            },
            Token::Identifier(_) => TokenType::Identifier,
            Token::Int(_) => TokenType::Int,
            Token::Float(_) => TokenType::Float,
            Token::Equal => TokenType::Equal,
            Token::Rpar => TokenType::Rpar,
            Token::Lpar => TokenType::Lpar,
            Token::Comma => TokenType::Comma,
            Token::Null => TokenType::Null,
            Token::Bool(_) => TokenType::Bool,
            Token::Lbracket => TokenType::Lbracket,
            Token::Rbracket => TokenType::Rbracket,
            Token::Quote => TokenType::Quote,
            Token::Whitespace => TokenType::Whitespace,
            Token::If => TokenType::If,
            Token::Else => TokenType::Else,
            Token::Then => TokenType::Then,
            Token::While => TokenType::While,
            Token::Do => TokenType::Do,
            Token::Ignore => TokenType::Ignore,
            Token::Lsb => TokenType::Lsb,
            Token::Rsb => TokenType::Rsb,
            _ => TokenType::Null,
        }
    }
}
#[cfg(test)]
mod test {
    use super::{Token, TokenType};

    #[test]
    fn test_token_type_operators_plus() {
        let expected = TokenType::Plus;
        let value = Token::Ope(super::Operator::Plus).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_minus() {
        let expected = TokenType::Minus;
        let value = Token::Ope(super::Operator::Minus).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_mult() {
        let expected = TokenType::Multiplication;
        let value = Token::Ope(super::Operator::Multiplication).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_divide() {
        let expected = TokenType::Divide;
        let value = Token::Ope(super::Operator::Divide).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_expo() {
        let expected = TokenType::Expo;
        let value = Token::Ope(super::Operator::Expo).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_equality() {
        let expected = TokenType::Equality;
        let value = Token::Ope(super::Operator::Equality).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_greater() {
        let expected = TokenType::Greater;
        let value = Token::Ope(super::Operator::GreaterThan).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_lesser() {
        let expected = TokenType::Lesser;
        let value = Token::Ope(super::Operator::LesserThan).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_greaterq() {
        let expected = TokenType::GreaterEq;
        let value = Token::Ope(super::Operator::GreaterOrEqual).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_lesserq() {
        let expected = TokenType::LesserEq;
        let value = Token::Ope(super::Operator::LesserOrEqual).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_and() {
        let expected = TokenType::And;
        let value = Token::Ope(super::Operator::And).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_or() {
        let expected = TokenType::Or;
        let value = Token::Ope(super::Operator::Or).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_operators_not() {
        let expected = TokenType::Not;
        let value = Token::Ope(super::Operator::Not).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_identifier() {
        let expected = TokenType::Identifier;
        let value = Token::Identifier("s".to_string()).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_int() {
        let expected = TokenType::Int;
        let value = Token::Int(0).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_float() {
        let expected = TokenType::Float;
        let value = Token::Float(0.0).to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_equal() {
        let expected = TokenType::Equal;
        let value = Token::Equal.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_lpar() {
        let expected = TokenType::Lpar;
        let value = Token::Lpar.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_rpar() {
        let expected = TokenType::Rpar;
        let value = Token::Rpar.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_bool() {
        let expected = TokenType::Bool;
        let value = Token::Bool(false).to_token_type();
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
        let expected = TokenType::Comma;
        let value = Token::Comma.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_rbracket() {
        let expected = TokenType::Rbracket;
        let value = Token::Rbracket.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_lbracket() {
        let expected = TokenType::Lbracket;
        let value = Token::Lbracket.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_whitespace() {
        let expected = TokenType::Whitespace;
        let value = Token::Whitespace.to_token_type();
        assert_eq!(value, expected);
    }

    #[test]
    fn test_token_type_quote() {
        let expected = TokenType::Quote;
        let value = Token::Quote.to_token_type();
        assert_eq!(value, expected);
    }
}
