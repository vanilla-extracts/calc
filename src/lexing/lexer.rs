use std::str::FromStr;

use crate::lexing::token::Operator::*;
use crate::lexing::token::Token;
use crate::lexing::token::Token::PreAnd;

pub fn is_an_allowed_char(character: char) -> bool {
    character.is_alphanumeric()
        || character == '+'
        || character == '-'
        || character == '*'
        || character == '/'
        || character == '('
        || character == ')'
        || character == '}'
        || character == '{'
        || character == '"'
        || character == '.'
        || character == '='
        || character == '^'
        || character == ';'
        || character == ','
        || character == '!'
        || character == '<'
        || character == '>'
        || character == '|'
        || character == '&'
        || character == '['
        || character == ']'
        || character == '_'
        || character == '"'
        || character == ' '
}

fn lex_int(
    current_char: char,
    chars: &mut Vec<char>,
    current_pos: usize,
    len: usize,
) -> (i64, usize) {
    let (a, b) = lex_raddix(current_char, chars, current_pos, len);
    let err = i64::from_str(&a);
    if err.is_err() {
        (0, b)
    } else {
        (err.unwrap(), b)
    }
}

fn lex_raddix(
    mut current_char: char,
    chars: &mut Vec<char>,
    mut current_pos: usize,
    len: usize,
) -> (String, usize) {
    let mut str: String = String::new();
    while current_pos < len && (current_char.is_ascii_digit()) {
        str += &*current_char.to_string();

        current_pos += 1;
        let a = chars.get(current_pos);
        match a {
            Some(t) => current_char = *t,
            None => break,
        }
    }
    (str, current_pos)
}

fn lex_string(
    mut current_char: char,
    chars: &mut Vec<char>,
    mut current_pos: usize,
    len: usize,
) -> (String, usize) {
    let mut str: String = String::new();
    while current_pos < len && (current_char.is_alphanumeric() || current_char == '_') {
        str += &*current_char.to_string();

        current_pos += 1;
        let a = chars.get(current_pos);
        match a {
            Some(t) => current_char = *t,
            None => break,
        }
    }
    (str, current_pos)
}

fn lex_float(
    whole_side: i64,
    chars: &mut Vec<char>,
    mut current_pos: usize,
    len: usize,
) -> (f64, usize) {
    current_pos += 1;
    let current_char_options = chars.get(current_pos);
    let current_char = current_char_options.unwrap_or(&'0');
    let (a, b) = lex_raddix(*current_char, chars, current_pos, len);
    let f = f64::from_str(&(whole_side.to_string().as_str().to_owned() + "." + a.as_str()));
    if f.is_err() {
        return (f64::NAN, b);
    }
    (f.unwrap(), b)
}

pub fn lex(input: String) -> Vec<Token> {
    let mut vec: Vec<Token> = Vec::new();

    let mut current_pos = 0;

    let mut chars = input.as_str().chars().collect::<Vec<char>>();

    let mut quote_i = 0;

    let length = input.len();
    while current_pos < input.len() {
        let peeking_char = chars.get(current_pos);

        let current_character: char = match peeking_char {
            None => {
                current_pos += 1;
                continue;
            }
            Some(t) => *t,
        };
        if !is_an_allowed_char(current_character) {
            current_pos += 1;
            continue;
        };

        match current_character {
            '+' => match vec.pop() {
                Some(Token::Ope(Plus)) => {
                    vec.push(Token::Ope(ConcatOperation));
                    current_pos += 1;
                }
                Some(p) => {
                    vec.push(p);
                    vec.push(Token::Ope(Plus));
                    current_pos += 1;
                }
                None => {
                    vec.push(Token::Ope(Plus));
                    current_pos += 1;
                }
            },
            '-' => {
                vec.push(Token::Ope(Minus));
                current_pos += 1
            }
            '*' => {
                vec.push(Token::Ope(Multiplication));
                current_pos += 1
            }
            '/' => {
                vec.push(Token::Ope(Divide));
                current_pos += 1
            }
            ')' => {
                vec.push(Token::Rpar);
                current_pos += 1
            }
            '(' => {
                vec.push(Token::Lpar);
                current_pos += 1
            }
            '{' => {
                vec.push(Token::Lsb);
                current_pos += 1
            }
            '}' => {
                vec.push(Token::Rsb);
                current_pos += 1
            }
            '>' => {
                vec.push(Token::Ope(GreaterThan));
                current_pos += 1
            }
            '<' => {
                vec.push(Token::Ope(LesserThan));
                current_pos += 1
            }
            '"' => {
                vec.push(Token::Quote);
                quote_i += 1;
                current_pos += 1
            }
            ';' => {
                vec.push(Token::Ignore);
                current_pos += 1
            }
            '=' => match vec.pop() {
                Some(Token::Equal) => {
                    vec.push(Token::Ope(Equality));
                    current_pos += 1
                }
                Some(Token::Ope(LesserThan)) => {
                    vec.push(Token::Ope(LesserOrEqual));
                    current_pos += 1;
                }
                Some(Token::Ope(GreaterThan)) => {
                    vec.push(Token::Ope(GreaterOrEqual));
                    current_pos += 1;
                }
                Some(p) => {
                    vec.push(p);
                    vec.push(Token::Equal);
                    current_pos += 1
                }
                None => {
                    vec.push(Token::Equal);
                    current_pos += 1
                }
            },
            '&' => match vec.pop() {
                Some(Token::PreAnd) => {
                    vec.push(Token::Ope(And));
                    current_pos += 1;
                }
                Some(p) => {
                    vec.push(p);
                    vec.push(Token::PreAnd);
                    current_pos += 1;
                }
                _ => {
                    vec.push(Token::PreAnd);
                    current_pos += 1;
                }
            },
            '|' => match vec.pop() {
                Some(Token::PreOr) => {
                    vec.push(Token::Ope(Or));
                    current_pos += 1;
                }
                Some(p) => {
                    vec.push(p);
                    vec.push(Token::PreOr);
                    current_pos += 1;
                }
                _ => {
                    vec.push(Token::PreOr);
                    current_pos += 1;
                }
            },
            '^' => {
                vec.push(Token::Ope(Expo));
                current_pos += 1
            }
            ',' => {
                vec.push(Token::Comma);
                current_pos += 1
            }
            '!' => {
                vec.push(Token::Ope(NOT));
                current_pos += 1
            }
            ']' => {
                vec.push(Token::Rbracket);
                current_pos += 1
            }
            '[' => {
                vec.push(Token::Lbracket);
                current_pos += 1
            }
            ' ' => {
                if quote_i % 2 == 1 {
                    vec.push(Token::Whitespace);
                }
                current_pos += 1
            }
            '.' => {
                vec.push(Token::Ope(Selection));
                current_pos += 1
            }
            ch => {
                if ch.is_numeric() {
                    let (a, b) = lex_int(current_character, &mut chars, current_pos, length);
                    current_pos = b;
                    let cha = chars.get(current_pos);
                    match cha {
                        Some(char) => {
                            if *char == '.' {
                                let (a1, b1) = lex_float(a, &mut chars, current_pos, length);
                                current_pos = b1;
                                vec.push(Token::Float(a1))
                            } else {
                                vec.push(Token::Int(a));
                                current_pos = b;
                            }
                        }
                        None => {
                            vec.push(Token::Int(a));
                            current_pos = b;
                        }
                    }
                }
                if ch.is_alphabetic() || ch == '_' {
                    let (a, b) = lex_string(current_character, &mut chars, current_pos, length);
                    current_pos = b;
                    match a.as_str() {
                        "false" => vec.push(Token::Bool(false)),
                        "true" => vec.push(Token::Bool(true)),
                        "or" => vec.push(Token::Ope(Or)),
                        "and" => vec.push(Token::Ope(And)),
                        "geq" => vec.push(Token::Ope(GreaterOrEqual)),
                        "leq" => vec.push(Token::Ope(LesserOrEqual)),
                        "lt" => vec.push(Token::Ope(LesserThan)),
                        "gt" => vec.push(Token::Ope(GreaterThan)),
                        "eq" => vec.push(Token::Ope(Equality)),
                        "if" => vec.push(Token::If),
                        "then" => vec.push(Token::Then),
                        "else" => vec.push(Token::Else),
                        "while" => vec.push(Token::While),
                        "do" => vec.push(Token::Do),
                        _ => vec.push(Token::Identifier(a)),
                    }
                }
                if ch == '.' {
                    let (a, b) = lex_float(0, &mut chars, current_pos, length);
                    current_pos = b;
                    vec.push(Token::Float(a))
                }
            }
        }
    }
    let mut result = Vec::new();
    vec.iter()
        .filter(|x| x != &&Token::PreOr && x != &&PreAnd)
        .for_each(|x1| result.push(x1.clone()));
    result
}

#[cfg(test)]
mod tests {
    use crate::lexing::lexer::lex;
    use crate::lexing::token::Operator::*;
    use crate::lexing::token::Token::*;

    #[test]
    fn lex_plus() {
        let expected = vec![Ope(Plus)];
        let result = lex("+".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_minus() {
        let expected = vec![Ope(Minus)];
        let result = lex("-".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_mult() {
        let expected = vec![Ope(Multiplication)];
        let result = lex("*".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_divide() {
        let expected = vec![Ope(Divide)];
        let result = lex("/".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_operators() {
        let expected = vec![Ope(Plus), Ope(Multiplication), Ope(Minus), Ope(Divide)];
        let result = lex("+*-/".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_lpar() {
        let expected = vec![Lpar];
        let result = lex("(".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_rpar() {
        let expected = vec![Rpar];
        let result = lex(")".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_equal() {
        let expected = vec![Equal];
        let result = lex("=".to_string());
        assert_eq!(result, expected);
    }

    #[test]
    fn lex_tokens() {
        let expected = vec![Lpar, Rpar, Equal];
        let result = lex("()=".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn lex_simple_int() {
        let expected = vec![Int(1)];
        let result = lex("1".to_string());
        assert_eq!(result, expected);
    }

    #[test]
    fn lex_complex_int() {
        let expected = vec![Int(100)];
        let result = lex("100".to_string());
        assert_eq!(result, expected);
    }

    #[test]
    fn lex_simple_string() {
        let expected = vec![Identifier("test".to_string())];
        let result = lex("test".to_string());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_complex_operation() {
        let expected = vec![Int(1), Ope(Plus), Int(1)];
        let result = lex("1 + 1".to_string());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_complex_equality() {
        let expected = vec![Identifier("var1".to_string()), Equal, Int(100)];
        let result = lex("var1 = 100".to_string());
        assert_eq!(result, expected)
    }

    #[test]
    fn test_simple_float() {
        let expected = vec![Float(0.14)];
        let result = lex("0.14".to_string());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_complex_float() {
        let expected = vec![Float(314.05)];
        let result = lex("314.05".to_string());
        assert_eq!(result, expected)
    }
}
