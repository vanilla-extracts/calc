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

/// # Ram
/// The Ram is the current state of the variables of Calc
/// It is a Hashmap with the name (string) of the variable as key and the value (parameter) of the variable as value.
pub type Ram = HashMap<String, Parameters>;

/// # Functions
/// Functions is the current state of the user-defined functions of Calc
/// It is a Hashmap with the name (string) of the function as key and a couple of the list of arguments (Vec<Ast>) and the body (Ast) of the function as value.
pub type Functions = HashMap<String, (Vec<Ast>, Ast)>;

/// # Parameters
/// List of all possible parameters in Calc.
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
    SelectionOperation,
    ConcatOperation,
    Equal,
    Not,
    Assign,
    Null,
    ExpoOperation,
    Vector(Vec<Ast>),
    InterpreterVector(Vec<Parameters>),
    Var(Box<Parameters>, i64, String),
    Plus(Box<Parameters>, Box<Parameters>),
    Mul(Box<Parameters>, Box<Parameters>),
    Div(Box<Parameters>, Box<Parameters>),
    Call(String, Box<Parameters>),
}

/// # Ast
/// The Abstract Syntax Tree of Calc
/// The tree is binary and is either empty (Nil) or:
/// A node (prefix: parameter, left: Ast, right: Ast) or:
/// A function call (name: String, list of arguments: Vec<Ast>) or:
/// A condition (condition: Ast, then: Ast, else: Ast) or:
/// A while statement (condition: Ast, body: Ast, right: Nil) or:
/// An ignore statement (value: nothing, left: Ast, right: Ast)
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
    Conditional {
        condition: Box<Ast>,
        then_branch: Box<Ast>,
        else_branch: Box<Ast>,
    },
    While {
        condition: Box<Ast>,
        body: Box<Ast>,
    },
    Ignore {
        left: Box<Ast>,
        right: Box<Ast>,
    },
}

/// # Superscript
/// Transforms an integer to a string of the integer as a superscript (exponent)
/// Takes an integer as a sixty four bit integer
/// Returns the integer as a string as a superscript.
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
        .map(digit_to_superscript_char)
        .for_each(|f| vec.push(f));

    let i = vec.join("");
    if i == *"⁰" {
        "error".to_string()
    } else if i == "¹" {
        "".to_string()
    } else {
        i
    }
}

/// # Display for Parameters
/// Implements the Display trait for the Parameters enum.
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
            ConcatOperation => write!(f, "++"),
            SelectionOperation => write!(f, "."),
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

/// # Display for Ast
/// Implements the Display trait for the Ast enum.
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
                write!(f, "{}({})", v, vs.join(","))
            }
            Ast::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {condition} then {then_branch} else {else_branch}")
            }
            Ast::While { condition, body } => {
                write!(f, "while {condition} do {body}")
            }
            Ast::Ignore { left, right } => {
                write!(f, "{left}; {right}")
            }
        }
    }
}

impl Parameters {
    /// # Pretty Print Parameters
    /// Returns a "pretty printed" string of a parameter
    /// Takes a ref of itself
    /// Takes an Option of a mutable ref of the state of the variables of Calc
    /// Takes an Option of a mutable ref of the state of the user-defined functions of Calc
    /// Returns the pretty printed string of the parameter.
    pub fn pretty_print(
        &self,
        mut ram: Option<&mut Ram>,
        mut function: Option<&mut Functions>,
    ) -> String {
        match self {
            Identifier(s) => {
                if s.starts_with("@") {
                    match s.strip_prefix("@") {
                        None => String::new(),
                        Some(c) => {
                            format!("{} {}", Color::Purple.paint("Error:"), Color::Red.paint(c))
                        }
                    }
                } else if ram.is_none() {
                    self.to_string()
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
                        Float(f) if (1.0 - 1e-10..=1.0 + 1e-10).contains(&f) => {
                            if division {
                                "1"
                            } else {
                                ""
                            }
                        }
                        Rational(r) if r == Rationals::new(1, 1) => {
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
                        Float(f) if (-1.0 - 1e-10..=-1.0 + 1e-10).contains(&f) => {
                            if division {
                                "-1"
                            } else {
                                ""
                            }
                        }
                        Rational(r) if r == Rationals::new(-1, 1) => {
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
                        if l == "¹" || l == "⁻¹" {
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
                        if y_printed == *"0" {
                            x_printed.to_string()
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
                if vec.is_empty() {
                    return String::new();
                }
                if let Parameters::InterpreterVector(_) = lst.first().unwrap() {
                    matrix = true
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
    /// Argument Print
    /// Returns a pretty string of a parameter if it is the final value.
    /// Takes a ref to itself
    /// Takes an Option of a mutable reference of the state of the variables of Calc
    /// Takes an Option of a mutable reference of the state of the user-defined functions of Calc
    /// Returns the pretty string of itself.
    pub fn argument_print(
        &self,
        ram: Option<&mut Ram>,
        function: Option<&mut Functions>,
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
                        Color::Cyan.paint(s.clone().to_string()),
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

/// # Token To Parameters
/// Gives the Parameter equivalent of a token
/// Takes a reference to a Token
/// Returns the parameter equivalent of the input token.
pub fn token_to_parameter(token: &Token) -> Parameters {
    match token {
        Token::Int(i) => Int(*i),
        Token::Float(f) => Float(*f),
        Token::Identifier(s) => Identifier(s.clone()),
        Token::Ope(Operator::Plus) => PlusOperation,
        Token::Ope(Operator::Minus) => MinusOperation,
        Token::Ope(Operator::Multiplication) => MultiplicationOperation,
        Token::Ope(Operator::Divide) => DivideOperation,
        Token::Ope(Operator::Expo) => ExpoOperation,
        Token::Ope(Operator::Equality) => Equal,
        Token::Ope(Operator::GreaterOrEqual) => GreaterOrEqualOperation,
        Token::Ope(Operator::GreaterThan) => GreaterOperation,
        Token::Ope(Operator::LesserThan) => LesserOperation,
        Token::Ope(Operator::LesserOrEqual) => LesserOrEqualOperation,
        Token::Ope(Operator::Not) => Not,
        Token::Ope(Operator::Or) => OrOperation,
        Token::Ope(Operator::And) => AndOperation,
        Token::Ope(Operator::Selection) => SelectionOperation,
        Token::Ope(Operator::ConcatOperation) => ConcatOperation,
        Token::Equal => Assign,
        Token::Bool(b) => Bool(*b),
        Token::Rbracket => Vector(Vec::new()),
        _ => Null,
    }
}

impl Parameters {
    /// # Abs
    /// Computes the absolute value of a parameter
    /// Takes itself
    /// Takes an Option of a mutable reference of the state of the variables of Calc
    /// Returns the absolute value of the input parameter, as a parameter.
    pub fn abs(self, ram: Option<&Ram>) -> Parameters {
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
                        Some(t) => t.clone().abs(ram),
                    }
                }
            },
            _ => Parameters::Null,
        }
    }
}

impl Ast {
    pub fn new(p: Parameters) -> Self {
        Ast::Node {
            value: p,
            left: Box::from(Ast::Nil),
            right: Box::from(Ast::Nil),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parsing::ast::{Ast, Parameters};

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
