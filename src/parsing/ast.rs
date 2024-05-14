use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::exact_math::rationals::Rationals;
use crate::lexing::token::{Operator, Token};
use crate::parsing::ast::Ast::{Nil, Node};
use crate::parsing::ast::Parameters::*;
use crate::utils::matrix_utils::transpose;

#[derive(Debug, Clone, PartialEq)]
pub enum Parameters {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Identifier(String),
    Rational(Rationals),
    PlusOperation,
    MinusOperation,
    MultiplicationOperation,
    DivideOperation,
    LesserOrEqualOperation,
    LesserOperation,
    GreaterOrEqualOperation,
    GreaterOperation,
    OrOperation,
    AndOperation,
    Equal,
    Not,
    Assign,
    Null,
    ExpoOperation,
    Vector(Box<Vec<Ast>>),
    InterpreterVector(Box<Vec<Parameters>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Nil,
    Node {
        value: Parameters,
        left: Box<Ast>,
        right: Box<Ast>,
    },
    Call {
        name: String,
        lst: Vec<Ast>,
    },
}

impl Display for Parameters {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Int(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{:.10}", fl),
            Identifier(s) => write!(f, "{}", s),
            PlusOperation => write!(f, "+"),
            MinusOperation => write!(f, "-"),
            MultiplicationOperation => write!(f, "*"),
            DivideOperation => write!(f, "/"),
            Assign => write!(f, "="),
            Null => write!(f, ""),
            ExpoOperation => write!(f, "^"),
            GreaterOperation => write!(f, ">"),
            LesserOperation => write!(f, "<"),
            GreaterOrEqualOperation => write!(f, ">="),
            LesserOrEqualOperation => write!(f, "<="),
            Equal => write!(f, "=="),
            Not => write!(f, "!"),
            Bool(b) => write!(f, "{b}"),
            AndOperation => write!(f, "&&"),
            OrOperation => write!(f, "||"),
            Vector(a) => write!(f, "{:?}", a),
            InterpreterVector(a) => write!(f, "{:?}", a),
            Str(s) => write!(f, "{s}"),
            Rational(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nil => write!(f, ""),
            Node {
                value: v,
                left: l,
                right: r,
            } => {
                write!(f, "({} {} {})", l, v, r)
            }
            Ast::Call { name: v, lst: s } => {
                let mut vs = Vec::new();
                s.iter().for_each(|x1| vs.push(x1.to_string()));
                write!(f, "{}({})", v, vs.join(",").to_string())
            }
        }
    }
}

impl Parameters {
    pub fn pretty_print(
        &self,
        mut ram: Option<&mut HashMap<String, Parameters>>,
        mut function: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
    ) -> String {
        match self {
            Identifier(s) => {
                if s.starts_with("@") {
                    match s.strip_prefix("@") {
                        None => format!(""),
                        Some(c) => format!("{c}"),
                    }
                } else {
                    if ram == None {
                        return self.to_string();
                    } else {
                        match ram.as_mut().unwrap().get(s) {
                            None => "This variable is not initialized yet".to_string(),
                            Some(t) => t.clone().pretty_print(
                                Some(ram.as_mut().unwrap()),
                                Some(function.as_mut().unwrap()),
                            ),
                        }
                    }
                }
            }
            InterpreterVector(lst) => {
                let mut vec = Vec::new();

                lst.iter()
                    .map(|x| {
                        x.pretty_print(
                            Some(&mut ram.as_deref().unwrap().clone()),
                            Some(&mut function.as_deref().unwrap().clone()),
                        )
                    })
                    .for_each(|x| vec.push(x));
                /*-------------
                 * |1 2 3 4 5 6 |
                 * -------------
                 */
                let mut matrix = false;
                if vec.len() == 0 {
                    return format!("");
                }
                match lst.first().unwrap() {
                    Parameters::InterpreterVector(_) => matrix = true,
                    _ => (),
                }
                if !matrix {
                    format!("|{}|", vec.join(" "))
                } else {
                    let mut vss = Vec::new();
                    let mut max_size = 0;
                    vec.clone()
                        .into_iter()
                        .for_each(|x| vss.push(x[1..(x.len() - 1)].to_string()));
                    vec.into_iter().for_each(|x| {
                        if x.len() > max_size {
                            max_size = x.len()
                        }
                    });

                    let mut matrix = Vec::new();
                    for el in vss.into_iter() {
                        let mut col = Vec::new();
                        let v = el.split_whitespace();
                        for i in v {
                            col.push(i.to_string());
                        }
                        matrix.push(col);
                    }

                    let mut final_v = Vec::new();
                    let cols = transpose(matrix.clone());

                    for x in cols {
                        let mut max_size = 0;
                        x.clone().into_iter().for_each(|y| {
                            if y.len() > max_size {
                                max_size = y.len()
                            }
                        });

                        let mut new_line = Vec::new();

                        for y in x.clone() {
                            let vs = vec![" "; (max_size - y.len()) / 2];
                            let vs2 = vec![" "; (max_size - y.len()) - vs.len()];
                            new_line.push(format!("{}{}{}", vs2.join(""), y, vs.join("")));
                        }

                        final_v.push(new_line);
                    }

                    let vfinal = transpose(final_v);

                    let mut max_length = 0;

                    let mut v_final = Vec::new();
                    vfinal.into_iter().for_each(|x| v_final.push(x.join(" ")));

                    v_final.clone().into_iter().for_each(|x| {
                        if x.len() > max_length {
                            max_length = x.len()
                        }
                    });

                    let first_line = vec!["-"; max_length];
                    let s = format!(
                        "+{}+\n|{}|\n+{}+",
                        first_line.join(""),
                        v_final.join("|\n|"),
                        first_line.join("")
                    );
                    s
                }
            }
            _ => format!("{self}"),
        }
    }
}

pub fn token_to_parameter(token: Token) -> Parameters {
    match token {
        Token::INT(i) => Int(i),
        Token::FLOAT(f) => Float(f),
        Token::IDENTIFIER(s) => Identifier(s),
        Token::OPE(Operator::PLUS) => PlusOperation,
        Token::OPE(Operator::MINUS) => MinusOperation,
        Token::OPE(Operator::MULTIPLICATION) => MultiplicationOperation,
        Token::OPE(Operator::DIVIDE) => DivideOperation,
        Token::OPE(Operator::EXPO) => ExpoOperation,
        Token::OPE(Operator::EQUALITY) => Equal,
        Token::OPE(Operator::GreaterOrEqual) => GreaterOrEqualOperation,
        Token::OPE(Operator::GreaterThan) => GreaterOperation,
        Token::OPE(Operator::LesserThan) => LesserOperation,
        Token::OPE(Operator::LesserOrEqual) => LesserOrEqualOperation,
        Token::OPE(Operator::NOT) => Not,
        Token::OPE(Operator::Or) => OrOperation,
        Token::OPE(Operator::And) => AndOperation,
        Token::EQUAL => Assign,
        Token::BOOL(b) => Bool(b),
        Token::RBRACKET => Vector(Box::from(Vec::new())),
        _ => Null,
    }
}

impl Parameters {
    pub fn abs(self, ram: Option<&HashMap<String, Parameters>>) -> Parameters {
        match self {
            Parameters::Int(i) => Parameters::Int(i.abs()),
            Parameters::Float(f) => Parameters::Float(f.abs()),
            Parameters::Rational(r) => Parameters::Rational(r.abs()),
            Parameters::Identifier(s) => match ram {
                None => Parameters::Null,
                Some(t) => {
                    let param = t.get(&s);
                    match param {
                        None => Parameters::Null,
                        Some(t) => t.clone().abs(ram.as_deref()),
                    }
                }
            },
            _ => Parameters::Null,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parsing::ast::{Ast, Parameters};

    impl Ast {
        pub fn new(p: Parameters) -> Self {
            Ast::Node {
                value: p,
                left: Box::from(Ast::Nil),
                right: Box::from(Ast::Nil),
            }
        }
    }
    #[test]
    pub fn test_new() {
        let expected = Ast::Node {
            value: Parameters::Int(2),
            left: Box::from(Ast::Nil),
            right: Box::from(Ast::Nil),
        };
        let result = Ast::new(Parameters::Int(2));
        assert_eq!(result, expected)
    }
}
