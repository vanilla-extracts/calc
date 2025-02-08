use crate::exact_math::rationals::Rationals;
use crate::functions::add::add;
use crate::functions::divide::divide;
use crate::functions::expo::expo;
use crate::functions::function::*;
use crate::functions::minus::minus;
use crate::functions::mult::mult;
use crate::interpreting::stdlib::exec;
use crate::parsing::ast::{Ast, Functions, Parameters, Ram};

pub fn interpret(ast: &Ast, mut ram: &mut Ram, mut function: &mut Functions) -> Parameters {
    match ast {
        Ast::Nil => Parameters::Null,
        Ast::Node {
            value: v,
            left: l,
            right: r,
        } => {
            let param1 = interpret(l, &mut ram, &mut function);
            let param2 = interpret(r, &mut ram, &mut function);
            let last = match v {
                Parameters::PlusOperation => add(param1, param2, Some(&ram)),
                Parameters::MinusOperation => minus(param1, param2, Some(&ram)),
                Parameters::MultiplicationOperation => mult(param1, param2, Some(&ram)),
                Parameters::DivideOperation => divide(param1, param2, Some(&ram)),
                Parameters::ExpoOperation => expo(param1, param2, Some(&ram)),
                Parameters::Equal => equal(param1, param2, Some(&ram)),
                Parameters::Not => not(param1, param2, Some(&ram)),
                Parameters::GreaterOperation => greater(param1, param2, Some(&ram)),
                Parameters::GreaterOrEqualOperation => greater_or_equal(param1, param2, Some(&ram)),
                Parameters::LesserOperation => lesser(param1, param2, Some(&ram)),
                Parameters::LesserOrEqualOperation => lesser_or_equal(param1, param2, Some(&ram)),
                Parameters::AndOperation => and(param1, param2, Some(&ram)),
                Parameters::OrOperation => or(param1, param2, Some(&ram)),
                Parameters::Rational(s) => Parameters::Rational(s.clone()),
                Parameters::Str(s) => Parameters::Str(s.to_string()),
                Parameters::Assign => match *(l.clone()) {
                    Ast::Call { name: n, lst: list } => {
                        if function.contains_key(&n) {
                            println!(
                                "{}",
                                ansi_term::Color::Red
                                    .bold()
                                    .paint("This function has already been set")
                            );
                            Parameters::Null
                        } else {
                            if n.as_str() != "" {
                                (function).insert(n.to_string(), (list.clone(), *r.clone()));
                            }
                            println!(
                                "{}: {} = {}",
                                ansi_term::Color::Cyan.paint("fun"),
                                ansi_term::Color::RGB(255, 215, 0).paint(format!(
                                    "{}",
                                    Ast::Call {
                                        name: n.clone(),
                                        lst: list.clone()
                                    }
                                )),
                                ansi_term::Color::RGB(255, 215, 0).paint(format!("{}", *r.clone()))
                            );
                            Parameters::Null
                        }
                    }
                    _ => {
                        let p1 = match *l.clone() {
                            Ast::Node { value, left, right } => {
                                match (value.clone(), *left, *right) {
                                    (Parameters::Identifier(_), Ast::Nil, Ast::Nil) => {
                                        value.clone()
                                    }
                                    _ => Parameters::Null,
                                }
                            }
                            _ => Parameters::Null,
                        };
                        let (a, b) = assign(p1, param2.clone());
                        if a != "".to_string() {
                            if ram.contains_key(&a) {
                                ram.remove(&a);
                            }
                            (ram).insert(a.clone(), b.clone());

                            println!(
                                "{}: {} = {}",
                                ansi_term::Color::Cyan.paint("assign"),
                                ansi_term::Color::Yellow.paint(format!("{}", a.clone())),
                                ansi_term::Color::Yellow.paint(format!(
                                    "{}",
                                    b.clone().pretty_print(Some(ram), Some(function))
                                ))
                            );

                            return Parameters::Null;
                        }
                        Parameters::Null
                    }
                },
                Parameters::Float(f, _) => Parameters::Rational(Rationals::rationalize(*f)),
                Parameters::Int(i) => Parameters::Int(*i),
                Parameters::Identifier(s) => {
                    if ram.contains_key(s) {
                        ram.get(s).unwrap().clone()
                    } else {
                        Parameters::Identifier(s.clone())
                    }
                }
                Parameters::Bool(b) => Parameters::Bool(*b),
                Parameters::Null => Parameters::Null,
                Parameters::Vector(a) => {
                    let mut vec = Vec::new();
                    (*a).clone()
                        .into_iter()
                        .map(|a| interpret(&a, ram, function))
                        .for_each(|s| vec.push(s));
                    Parameters::InterpreterVector(Box::from(vec))
                }
                Parameters::InterpreterVector(a) => Parameters::InterpreterVector(a.clone()),
                Parameters::Var(x, y, z) => Parameters::Var(x.clone(), y.clone(), z.clone()),
                Parameters::Plus(x, y) => add(*x.clone(), *y.clone(), Some(&ram)),
                Parameters::Mul(x, y) => mult(*x.clone(), *y.clone(), Some(&ram)),
                Parameters::Div(x, y) => divide(*x.clone(), *y.clone(), Some(&ram)),
                Parameters::Call(x, y) => {
                    exec(x.clone(), vec![*y.clone()], Some(ram), Some(function))
                }
            };
            last.clone()
        }
        Ast::Call { name: n, lst: list } => {
            let v: Vec<Parameters> = list.iter().map(|x| interpret(x, ram, function)).collect();
            exec(n.to_string(), v, Some(&mut ram), Some(&mut function))
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::exact_math::rationals::Rationals;
    use crate::interpreting::interpreter::interpret;
    use crate::parsing::ast::{Ast, Parameters};

    #[test]
    fn test_interpreter_int() {
        let mut ram: HashMap<String, Parameters> = HashMap::new();
        let mut function: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
        let expected = Parameters::Int(2);
        let ast = Ast::Node {
            value: Parameters::Int(2),
            left: Box::from(Ast::Nil),
            right: Box::from(Ast::Nil),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }

    #[test]
    fn test_interpreter_float() {
        let mut ram: HashMap<String, Parameters> = HashMap::new();
        let mut function: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
        let expected = Parameters::Rational(Rationals::new(1, 2));
        let ast = Ast::Node {
            value: Parameters::Float(2.0),
            left: Box::from(Ast::Nil),
            right: Box::from(Ast::Nil),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }

    #[test]
    fn test_interpreter_plus_operation() {
        let mut ram: HashMap<String, Parameters> = HashMap::new();
        let mut function: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
        let expected = Parameters::Int(2);
        let ast = Ast::Node {
            value: Parameters::PlusOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }

    #[test]
    fn test_interpreter_minus_operation() {
        let mut ram: HashMap<String, Parameters> = HashMap::new();
        let mut function: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
        let expected = Parameters::Int(0);
        let ast = Ast::Node {
            value: Parameters::MinusOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }

    #[test]
    fn test_interpreter_mult_operation() {
        let mut ram: HashMap<String, Parameters> = HashMap::new();
        let mut function: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
        let expected = Parameters::Int(1);
        let ast = Ast::Node {
            value: Parameters::MultiplicationOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }

    #[test]
    fn test_interpreter_divide_operation() {
        let mut ram: HashMap<String, Parameters> = HashMap::new();
        let mut function: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
        let expected =
            Parameters::Rational(crate::exact_math::rationals::Rationals { under: 1, over: 1 });
        let ast = Ast::Node {
            value: Parameters::DivideOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }
}
