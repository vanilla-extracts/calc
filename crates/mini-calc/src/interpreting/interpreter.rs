use crate::interpreting::stdlib::exec;
use calc_lib::exact_math::float_mode::FloatMode;
use calc_lib::exact_math::rationals::Rationals;
use calc_lib::functions::add::add;
use calc_lib::functions::divide::divide;
use calc_lib::functions::expo::expo;
use calc_lib::functions::function::*;
use calc_lib::functions::minus::minus;
use calc_lib::functions::mult::mult;
use calc_lib::parsing::ast::{Ast, Functions, Parameters, Ram};
use calc_lib::FLOAT_MODE;

/// # Interpreter
/// Interprets the Ast, and gives the result
/// A really simple and intuitive interpreter, it deconstructs the input Ast
/// - If it is the empty tree it returns the null parameter
/// - If it is a `Node(v,lhs,rhs)` it interprets recursively `lhs` and `rhs`, then match the value `v`, and call the right function.
/// - If it is a function call, it interprets the whole list of parameters and call the function (be it std or user-defined)
/// - If it is a conditional, it interprets the condition and interprets the right branch
/// - If it is a while, it interprets the condition then run the while and returns it as a vector of parameters
/// - If it is an ignore, it interprets `lhs`, ignores it, and interprets `rhs`, returns it.
///
/// Takes a reference to the Ast
/// Takes a mutable reference to the state of the variables of Calc (see: [Ram](../parsing/ast.rs))
/// Takes a mutable reference to the state of the user-defined functions of Calc (see [Functions](../parsing/ast.rs))
/// Returns the final parameter.
pub fn interpret(ast: &Ast, mut ram: &mut Ram, mut function: &mut Functions) -> Parameters {
    match ast {
        Ast::Nil => Parameters::Null,
        Ast::Node {
            value: v,
            left: l,
            right: r,
        } => {
            let param1 = interpret(l, ram, function);
            let param2 = interpret(r, ram, function);
            let last = match v {
                Parameters::PlusOperation => add(param1, param2, Some(ram)),
                Parameters::MinusOperation => minus(param1, param2, Some(ram)),
                Parameters::MultiplicationOperation => mult(param1, param2, Some(ram)),
                Parameters::DivideOperation => divide(param1, param2, Some(ram)),
                Parameters::ExpoOperation => expo(param1, param2, Some(ram)),
                Parameters::Equal => equal(param1, param2, Some(ram)),
                Parameters::Not => not(param1, param2, Some(ram)),
                Parameters::GreaterOperation => greater(param1, param2, Some(ram)),
                Parameters::GreaterOrEqualOperation => greater_or_equal(param1, param2, Some(ram)),
                Parameters::LesserOperation => lesser(param1, param2, Some(ram)),
                Parameters::LesserOrEqualOperation => lesser_or_equal(param1, param2, Some(ram)),
                Parameters::AndOperation => and(param1, param2, Some(ram)),
                Parameters::OrOperation => or(param1, param2, Some(ram)),
                Parameters::SelectionOperation => select(param1, param2, Some(ram)),
                Parameters::ConcatOperation => concat(param1, param2, Some(ram)),
                Parameters::Rational(s) => Parameters::Rational(*s),
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
                        if a != *"" {
                            if ram.contains_key(&a) {
                                ram.remove(&a);
                            }
                            (ram).insert(a.clone(), b.clone());

                            return Parameters::Null;
                        }
                        Parameters::Null
                    }
                },
                Parameters::Float(f) => FLOAT_MODE.with(|fm| match *fm.borrow() {
                    FloatMode::Exact => Parameters::Rational(Rationals::rationalize(*f)),
                    _ => Parameters::Float(*f),
                }),
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
                    Parameters::InterpreterVector(vec)
                }
                Parameters::InterpreterVector(a) => Parameters::InterpreterVector(a.clone()),
                Parameters::Var(x, y, z) => Parameters::Var(x.clone(), *y, z.clone()),
                Parameters::Plus(x, y) => add(*x.clone(), *y.clone(), Some(ram)),
                Parameters::Mul(x, y) => mult(*x.clone(), *y.clone(), Some(ram)),
                Parameters::Div(x, y) => divide(*x.clone(), *y.clone(), Some(ram)),
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
        Ast::Conditional {
            condition,
            then_branch,
            else_branch,
        } => {
            if let Parameters::Bool(condition_bool) = interpret(condition, ram, function) {
                if condition_bool {
                    interpret(then_branch, ram, function)
                } else {
                    interpret(else_branch, ram, function)
                }
            } else {
                Parameters::Identifier(
                    "@Runtime exception, condition did not collapse to a bool".to_string(),
                )
            }
        }
        Ast::While { condition, body } => {
            let mut vec = vec![];
            loop {
                if let Parameters::Bool(condition_bool) = interpret(condition, ram, function) {
                    if !condition_bool {
                        return Parameters::InterpreterVector(vec);
                    }
                    vec.push(interpret(body, ram, function));
                } else {
                    return Parameters::Identifier(
                        "@Runtime exception, condition did not collapse to a bool".to_string(),
                    );
                }
            }
        }
        Ast::Ignore { left, right } => {
            let _ = interpret(left, ram, function);
            interpret(right, ram, function)
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::interpreting::interpreter::interpret;
    use calc_lib::exact_math::rationals::Rationals;
    use calc_lib::parsing::ast::{Ast, Parameters};

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
            Parameters::Rational(calc_lib::exact_math::rationals::Rationals { under: 1, over: 1 });
        let ast = Ast::Node {
            value: Parameters::DivideOperation,
            left: Box::from(Ast::new(Parameters::Int(1))),
            right: Box::from(Ast::new(Parameters::Int(1))),
        };
        let result = interpret(&ast, &mut ram, &mut function);
        assert_eq!(result, expected)
    }
}
