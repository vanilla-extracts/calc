use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use ansi_term::Color;

use crate::exact_math::float_mode::FloatMode;
use crate::exact_math::rationals::Rationals;
use crate::exact_math::scientific_mode::from_float;
use crate::lexing::token::{Operator, Token};
use crate::parsing::ast::Ast::{Nil, Node};
use crate::parsing::ast::Parameters::*;
use crate::utils::matrix_utils::transpose;
use crate::FLOAT_MODE;

pub type Ram = HashMap<String, Parameters>;
pub type Functions = HashMap<String, (Vec<Ast>, Ast)>;

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
    Var(Box<Parameters>, i64, String),
    Plus(Box<Parameters>, Box<Parameters>),
    Mul(Box<Parameters>, Box<Parameters>),
    Div(Box<Parameters>, Box<Parameters>),
    Call(String, Box<Parameters>),
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

pub fn int_to_superscript_string(i: i64) -> String {
    fn digit_to_superscript_char(i: &str) -> &str {
        match i {
            "-" => "⁻",
            "0" => "⁰",
            "1" => "¹",
            "2" => "²",
            "3" => "³",
            "4" => "⁴",
            "5" => "⁵",
            "6" => "⁶",
            "7" => "⁷",
            "8" => "⁸",
            "9" => "⁹",
            _ => "",
        }
    }

    let mut vec = vec![];
    let string_int = i.to_string();
    string_int
        .split("")
        .map(|x| digit_to_superscript_char(x))
        .for_each(|f| vec.push(f));

    let i = vec.join("");
    if i == "⁰".to_string() {
        "error".to_string()
    } else if i == "¹" {
        "".to_string()
    } else {
        i
    }
}

impl Display for Parameters {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Int(i) => write!(f, "{}", i),
            Float(fs) => FLOAT_MODE.with(|fm| match *fm.borrow() {
                FloatMode::Normal => write!(f, "{:.10}", fs),
                FloatMode::Exact => write!(f, "{}", fs),
                FloatMode::Science => write!(f, "{}", from_float(*fs)),
            }),
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
            Plus(x, y) => write!(f, "(({x})+({y}))"),
            Mul(x, y) => write!(f, "(({x})*({y}))"),
            Var(x, y, s) => write!(f, "({x}){s}{}", int_to_superscript_string(*y)),
            Div(x, y) => write!(f, "(({x})/({y}))"),
            Call(x, y) => write!(f, "{x}({y})"),
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
        mut ram: Option<&mut Ram>,
        mut function: Option<&mut Functions>,
    ) -> String {
        match self {
            Identifier(s) => {
                if s.starts_with("@") {
                    match s.strip_prefix("@") {
                        None => format!(""),
                        Some(c) => {
                            format!("{} {}", Color::Purple.paint("Error:"), Color::Red.paint(c))
                        }
                    }
                } else {
                    if ram.is_none() {
                        return self.to_string();
                    } else {
                        match ram.as_mut().unwrap().get(s) {
                            None => s.to_string(),
                            Some(t) => t.clone().pretty_print(
                                Some(ram.as_mut().unwrap()),
                                Some(function.as_mut().unwrap()),
                            ),
                        }
                    }
                }
            }

            Var(x, y, z) => {
                let l = int_to_superscript_string(*y);
                if l == *"error".to_string() {
                    format!("{}", x.clone())
                } else {
                    let division = l.starts_with("⁻");
                    let separator = if division { "/" } else { "" };
                    let v = &x.pretty_print(
                        Some(ram.as_mut().unwrap()),
                        Some(function.as_mut().unwrap()),
                    );
                    let vs = format!("({v})");

                    let first_attach = match **x {
                        Int(1) => {
                            if division {
                                "1"
                            } else {
                                ""
                            }
                        }
                        Float(f) if f >= 1.0 - 1e-10 && f <= 1.0 + 1e-10 => {
                            if division {
                                "1"
                            } else {
                                ""
                            }
                        }
                        Rational(r) if r.clone() == Rationals::new(1, 1) => {
                            if division {
                                "1"
                            } else {
                                ""
                            }
                        }
                        Int(-1) => {
                            if division {
                                "-1"
                            } else {
                                "-"
                            }
                        }
                        Float(f) if f >= -1.0 - 1e-10 && f <= -1.0 + 1e-10 => {
                            if division {
                                "-1"
                            } else {
                                ""
                            }
                        }
                        Rational(r) if r.clone() == Rationals::new(-1, 1) => {
                            if division {
                                "-1"
                            } else {
                                ""
                            }
                        }
                        _ => vs.as_str(),
                    };
                    let e = l.replace("⁻", "");
                    format!(
                        "{}{}{}{}",
                        first_attach,
                        separator,
                        z,
                        if l == "¹" {
                            ""
                        } else if l == "⁻¹" {
                            ""
                        } else {
                            e.as_str()
                        }
                    )
                }
            }

            Mul(x, y) => {
                let x_printed = x.pretty_print(
                    Some(ram.as_mut().unwrap()),
                    Some(function.as_mut().unwrap()),
                );
                let y_printed = y.pretty_print(
                    Some(ram.as_mut().unwrap()),
                    Some(function.as_mut().unwrap()),
                );
                format!("({x_printed})*({y_printed})")
            }

            Plus(x, y) => {
                let mut x_printed = x.pretty_print(
                    Some(ram.as_mut().unwrap()),
                    Some(function.as_mut().unwrap()),
                );
                let y_printed = y.pretty_print(
                    Some(ram.as_mut().unwrap()),
                    Some(function.as_mut().unwrap()),
                );
                if x_printed == "0" {
                    x_printed = "".to_string()
                }
                match y_printed.chars().nth(0) {
                    Some('-') => format!("({}{})", x_printed, y_printed),
                    _ => {
                        if y_printed == "0".to_string() {
                            format!("{}", x_printed)
                        } else {
                            format!("({})+({})", x_printed, y_printed)
                        }
                    }
                }
            }

            Div(x, y) => {
                let x_printed = x.pretty_print(
                    Some(ram.as_mut().unwrap()),
                    Some(function.as_mut().unwrap()),
                );
                let y_printed = y.pretty_print(
                    Some(ram.as_mut().unwrap()),
                    Some(function.as_mut().unwrap()),
                );

                format!("({x_printed})/({y_printed})")
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
    pub fn argument_print(
        &self,
        ram: Option<&mut HashMap<String, Parameters>>,
        function: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
    ) -> String {
        match self.clone() {
            Int(_) => format!(
                "{}: {} = {}",
                Color::Cyan.paint("val"),
                Color::Green.paint("int"),
                Color::Green.paint(self.pretty_print(ram, function))
            ),
            Float(_) => {
                let val = self.pretty_print(ram, function);
                if val.contains("/") {
                    format!(
                        "{}: {} = {}",
                        Color::Cyan.paint("val"),
                        Color::RGB(237, 144, 144).paint("rational"),
                        Color::RGB(237, 144, 144).paint(val)
                    )
                } else {
                    format!(
                        "{}: {} = {}",
                        Color::Cyan.paint("val"),
                        Color::RGB(186, 214, 152).paint("float"),
                        Color::RGB(186, 214, 152).paint(val)
                    )
                }
            }
            Identifier(s) => {
                if s.starts_with("@") {
                    self.pretty_print(ram, function)
                } else {
                    format!(
                        "{}: {} = {}",
                        Color::Cyan.paint(format!("{}", s.clone())),
                        Color::Yellow.paint("ident"),
                        Color::Yellow.paint(self.pretty_print(ram, function))
                    )
                }
            }
            Rational(_) => format!(
                "{}: {} = {}",
                Color::Cyan.paint("val"),
                Color::RGB(237, 138, 35).paint("rational"),
                Color::RGB(237, 138, 35).paint(self.pretty_print(ram, function)),
            ),
            Bool(_) => format!(
                "{}: {} = {}",
                Color::Cyan.paint("val"),
                Color::RGB(234, 144, 144).paint("bool"),
                Color::RGB(234, 144, 144).paint(self.pretty_print(ram, function)),
            ),
            InterpreterVector(_) => {
                format!(
                    "{}: {} \n{}",
                    Color::Cyan.paint("val"),
                    Color::RGB(248, 204, 249).paint("matrix"),
                    Color::RGB(248, 204, 249).paint(self.pretty_print(ram, function))
                )
            }
            Var(_, _, _) => {
                format!(
                    "{}: {} = {}",
                    Color::Cyan.paint("val"),
                    Color::RGB(30, 154, 176).paint("var"),
                    Color::RGB(30, 154, 176).paint(self.pretty_print(ram, function))
                )
            }
            Plus(_, _) | Mul(_, _) | Div(_, _) => {
                format!(
                    "{}: {} = {}",
                    Color::Cyan.paint("val"),
                    Color::Red.paint("op"),
                    Color::Red.paint(self.pretty_print(ram, function))
                )
            }
            Str(_) => {
                format!(
                    "{}: {} = {}{}{}",
                    Color::Cyan.paint("val"),
                    Color::Blue.paint("string"),
                    Color::Blue.paint("\""),
                    Color::Blue.paint(self.pretty_print(ram, function)),
                    Color::Blue.paint("\"")
                )
            }

            _ => self.pretty_print(ram, function),
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
