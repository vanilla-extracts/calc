use std::collections::HashMap;
use std::f64::consts::{E, PI};
use std::slice::from_ref;

use gnuplot::{AxesCommon, Figure};

use crate::configuration::loader::{load, load_config, Config};
use crate::functions::divide::divide;
use crate::functions::minus::minus;
use crate::interpreting::interpreter::interpret;
use crate::parsing::ast::{self};
use crate::parsing::ast::{
    Ast,
    Parameters::{self, *},
};
use crate::utils::matrix_utils::{lup_decompose, lup_determinant, lup_invert, transpose};
use crate::utils::plot_utils::computes_lines;

use crate::functions::add::add as other_add;
use crate::functions::mult::mult;

type Ram<'a> = Option<&'a mut ast::Ram>;
type Functions<'a> = Option<&'a mut ast::Functions>;

/// # Exec
/// Executes a given function
/// Takes the name of the function
/// Takes the list of arguments
/// Takes an Option of a mutable reference of the state of the variables of Calc
/// Takes an Option of a mutable reference of the state of the user-defined functions of Calc
/// Return the result.
pub fn exec(s: String, lst: Vec<Parameters>, ram: Ram, functions: Functions) -> Parameters {
    match s.as_str() {
        "cos" => cos(lst.as_slice(), &ram),
        "sin" => sin(lst.as_slice(), &ram),
        "tan" => tan(lst.as_slice(), &ram),
        "cosh" => cosh(lst.as_slice(), &ram),
        "sinh" => sinh(lst.as_slice(), &ram),
        "tanh" => tanh(lst.as_slice(), &ram),
        "exp" => exp(lst.as_slice(), &ram),
        "acos" => acos(lst.as_slice(), &ram),
        "asin" => asin(lst.as_slice(), &ram),
        "atan" => atan(lst.as_slice(), &ram),
        "ln" => ln(lst.as_slice(), &ram),
        "log" => ln(lst.as_slice(), &ram),
        "sqrt" => sqrt(lst.as_slice(), &ram),
        "fact" => factorial(lst.as_slice(), &ram),
        "factorial" => factorial(lst.as_slice(), &ram),
        "abs" => abs(lst.as_slice(), &ram),
        "ceil" => ceil(lst.as_slice(), &ram),
        "floor" => floor(lst.as_slice(), &ram),
        "round" => round(lst.as_slice(), &ram),
        "norm" => norm(lst.as_slice(), &ram),
        "transpose_vector" => transpose_vectors(lst.as_slice(), &ram),
        "transpose" => transpose_matrices(lst.as_slice(), &ram),
        "det" => det_matrix(lst.as_slice(), &ram),
        "invert" => inverse_matrix(lst.as_slice(), &ram),
        "plot" => plot_fn(lst.as_slice(), &ram, &functions, false),
        "termplot" => plot_fn(lst.as_slice(), &ram, &functions, true),
        "diff" => diff(lst.as_slice(), &ram, &functions),
        "debug" => debug(lst.as_slice()),
        "print" => print(lst.as_slice()),
        "split" => split_string(lst.as_slice(), &ram),
        "join" => join_string(lst.as_slice(), &ram),
        s => {
            let mut sram: HashMap<String, Parameters> = HashMap::new();
            sram.insert("pi".to_string(), Float(PI));
            sram.insert("e".to_string(), Float(E));
            match functions.cloned() {
                None => Identifier("@This function is unknown".to_string()),
                Some(mut f) => {
                    let (vec, ast): (Vec<Ast>, Ast) = match f.get(s) {
                        None => {
                            return Identifier("@This function is unknown".to_string());
                        }
                        Some((a, b)) => (a.clone(), b.clone()),
                    };

                    let mut names = Vec::new();
                    for v in vec {
                        match v {
                            Ast::Nil => (),
                            Ast::Call { .. } => (),
                            Ast::Conditional { .. } => (),
                            Ast::While { .. } => (),
                            Ast::Ignore { .. } => (),
                            Ast::Node {
                                value: v,
                                left: _l,
                                right: _r,
                            } => {
                                if let Identifier(s) = v {
                                    names.push(s.clone())
                                }
                            }
                        }
                    }
                    names
                        .iter()
                        .zip(lst)
                        .filter(|(name, param)| match param {
                            Parameters::Identifier(s) => s.as_str() != name.as_str(),
                            _ => true,
                        })
                        .for_each(|(name, param)| {
                            sram.insert(name.to_string(), param);
                        });
                    interpret(&ast, &mut sram, &mut f)
                }
            }
        }
    }
}

/// # Debug
/// Prints in debug mode the list of arguments
/// Takes a reference to the list of arguments
/// Returns the null parameter
pub fn debug(p: &[Parameters]) -> Parameters {
    println!("{:#?}", p);
    Parameters::Null
}

/// # Print
/// Prints in normal mode each arguments
/// Takes a reference to the list of arguments
/// Returns the null parameter
pub fn print(p: &[Parameters]) -> Parameters {
    p.iter().for_each(|f| println!("{f}"));
    Parameters::Null
}

/// # Split
/// Splits a string
/// Takes a reference to the list of arguments
/// Takes a Ram (see above)
/// Returns the result
///
/// Requirements: at least one argument
/// - If there is no second argument, it returns the first argument, trimmed
pub fn split_string(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }
    let str = match p.first() {
        Some(Str(s)) => s.trim(),
        Some(Identifier(s)) => match ram {
            Some(ref t) => match t.get(s) {
                Some(Str(sa)) => sa.trim(),
                Some(_) => "",
                None => s.trim(),
            },
            None => s.trim(),
        },
        _ => "",
    };
    if str.is_empty() {
        return Null;
    }
    let separator = match p.get(1) {
        Some(Str(s)) => s.trim(),
        Some(Identifier(s)) => match ram {
            Some(ref t) => match t.get(s) {
                Some(Str(sa)) => sa.trim(),
                Some(_) => "",
                None => s.trim(),
            },
            None => s.trim(),
        },
        _ => "",
    };

    if separator.is_empty() {
        InterpreterVector(
            str.chars()
                .map(|f| Str(f.to_string()))
                .collect::<Vec<Parameters>>(),
        )
    } else {
        InterpreterVector(
            str.split(separator)
                .map(|f| Str(f.to_string()))
                .collect::<Vec<Parameters>>(),
        )
    }
}

/// # Join
/// Joins a list of string with a delimiter
/// Takes a reference to the list of parameters
/// Takes a reference to the Ram (see above)
/// Returns the joined string
///
/// Requirements: at least one argument
/// - The delimiter is the *last* argument
pub fn join_string(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }
    let delimiter = match p.last() {
        Some(Str(s)) => s,
        Some(Identifier(s)) => match ram {
            Some(ref t) => match t.get(s) {
                Some(Str(sa)) => sa,
                Some(_) => "",
                None => s,
            },
            None => s,
        },
        _ => "",
    };

    Str(p
        .iter()
        .map(|f| match f {
            Str(s) => s,
            Identifier(s) => match ram {
                Some(ref t) => match t.get(s) {
                    Some(Str(sa)) => sa,
                    Some(_) => "",
                    None => s,
                },
                None => s,
            },
            _ => "",
        })
        .collect::<Vec<&str>>()
        .join(delimiter)
        .trim()
        .to_string())
}

/// # Cos
/// Computes the cos of the input parameter
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram (see above)
/// Returns the cos of the input parameter
///
/// Requirements: at least one argument
/// - If there is a second argument, the input is assumed to be in degrees rather than in radian
/// - The first argument must be either an integer, a float, a rational number, or a vector of these arguments
pub fn cos(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i) as f64) * (PI / 180.0)
            } else {
                (*i) as f64
            };
            Float(fs.cos())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.cos())
        }
        Rational(s) => {
            let fs = if degrees {
                (*s).approx() * PI / 180.0
            } else {
                (*s).approx()
            };
            Float(fs.cos())
        }
        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).cos()
                } else {
                    (i as f64).cos()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).cos()
                } else {
                    f.cos()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).cos()
                } else {
                    s.approx().cos()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(cos(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(cos(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("cos".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("cos".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        cos(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        cos(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("cos".to_string(), Box::from(p.clone())),
    }
}

/// # Sin
/// Same as in cos, but for the sinus.
pub fn sin(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i) as f64) * (PI / 180.0)
            } else {
                (*i) as f64
            };
            Float(fs.sin())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.sin())
        }
        Rational(s) => {
            let fs = if degrees {
                (*s).approx() * PI / 180.0
            } else {
                (*s).approx()
            };
            Float(fs.sin())
        }
        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).sin()
                } else {
                    (i as f64).sin()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).sin()
                } else {
                    f.sin()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).sin()
                } else {
                    s.approx().sin()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(sin(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(sin(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("sin".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("sin".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        sin(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        sin(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("sin".to_string(), Box::from(p.clone())),
    }
}

/// # Tan
/// Same as in cos, but for the tan
pub fn tan(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i) as f64) * (PI / 180.0)
            } else {
                (*i) as f64
            };
            Float(fs.tan())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.tan())
        }
        Rational(s) => {
            let fs = if degrees {
                (*s).approx() * PI / 180.0
            } else {
                (*s).approx()
            };
            Float(fs.tan())
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).tan()
                } else {
                    (i as f64).tan()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).tan()
                } else {
                    f.tan()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).tan()
                } else {
                    s.approx().tan()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(tan(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(tan(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("tan".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("tan".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        tan(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        tan(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("tan".to_string(), Box::from(p.clone())),
    }
}

/// # Cosh
/// Same as cos, but for the hyperbolic cos
pub fn cosh(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i) as f64) * (PI / 180.0)
            } else {
                (*i) as f64
            };
            Float(fs.cosh())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.cosh())
        }
        Rational(s) => {
            let fs = if degrees {
                (*s).approx() * PI / 180.0
            } else {
                (*s).approx()
            };
            Float(fs.cosh())
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).cosh()
                } else {
                    (i as f64).cosh()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).cosh()
                } else {
                    f.cosh()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).cosh()
                } else {
                    s.approx().cosh()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(cosh(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(cosh(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("cosh".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("cosh".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        cosh(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        cosh(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("cosh".to_string(), Box::from(p.clone())),
    }
}

/// # Sinh
/// Same as in cos, but for the hyperbolic sinus
pub fn sinh(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i) as f64) * (PI / 180.0)
            } else {
                (*i) as f64
            };
            Float(fs.sinh())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.sinh())
        }
        Rational(s) => {
            let fs = if degrees {
                (*s).approx() * PI / 180.0
            } else {
                (*s).approx()
            };
            Float(fs.sinh())
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).sinh()
                } else {
                    (i as f64).sinh()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).sinh()
                } else {
                    f.sinh()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).sinh()
                } else {
                    s.approx().sinh()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(sinh(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(sinh(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("sinh".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("sinh".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        sinh(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        sinh(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("sinh".to_string(), Box::from(p.clone())),
    }
}

/// # Tanh
/// Same as in cos, but for the hyperbolic tan
pub fn tanh(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i) as f64) * (PI / 180.0)
            } else {
                (*i) as f64
            };
            Float(fs.tanh())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.tanh())
        }
        Rational(s) => {
            let fs = if degrees {
                (*s).approx() * PI / 180.0
            } else {
                (*s).approx()
            };
            Float(fs.tanh())
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).tanh()
                } else {
                    (i as f64).tanh()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).tanh()
                } else {
                    f.tanh()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).tanh()
                } else {
                    s.approx().tanh()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(tanh(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(tanh(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("tanh".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("tanh".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        tanh(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        tanh(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("tanh".to_string(), Box::from(p.clone())),
    }
}

/// # Acos
/// Same as in cos, but for acos.
pub fn acos(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            Float(if degrees {
                fs.acos() * (180.0 / PI)
            } else {
                fs.acos()
            })
        }
        Float(f) => Parameters::Float(if degrees {
            f.acos() * (180.0 / PI)
        } else {
            f.acos()
        }),
        Rational(s) => Parameters::Float(if degrees {
            (*s).approx().acos() * 180.0 / PI
        } else {
            (*s).approx().acos()
        }),

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    (i as f64).acos() * 180.0 / PI
                } else {
                    (i as f64).acos()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    f.acos() * 180.0 / PI
                } else {
                    f.acos()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    s.approx().acos() * 180.0 / PI
                } else {
                    s.approx().acos()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(acos(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(acos(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("acos".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("acos".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        acos(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        acos(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("acos".to_string(), Box::from(p.clone())),
    }
}

/// # Asin
/// Same as in cos, but for the asin
pub fn asin(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            Float(if degrees {
                fs.asin() * (180.0 / PI)
            } else {
                fs.asin()
            })
        }
        Float(f) => Parameters::Float(if degrees {
            f.asin() * (180.0 / PI)
        } else {
            f.asin()
        }),

        Rational(s) => Parameters::Float(if degrees {
            (*s).approx().asin() * (180.0 / PI)
        } else {
            (*s).approx().asin()
        }),

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    (i as f64).asin() * 180.0 / PI
                } else {
                    (i as f64).asin()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    f.asin() * 180.0 / PI
                } else {
                    f.asin()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    s.approx().asin() * 180.0 / PI
                } else {
                    s.approx().asin()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(asin(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(asin(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("asin".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("asin".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        asin(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        asin(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("asin".to_string(), Box::from(p.clone())),
    }
}

/// # Atan
/// Same as in cos, but for the atan
pub fn atan(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            Float(if degrees {
                fs.atan() * (180.0 / PI)
            } else {
                fs.atan()
            })
        }
        Float(f) => Parameters::Float(if degrees {
            f.atan() * (180.0 / PI)
        } else {
            f.atan()
        }),

        Rational(s) => Parameters::Float(if degrees {
            (*s).approx().atan() * (180.0 / PI)
        } else {
            (*s).approx().atan()
        }),

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if degrees {
                    (i as f64).atan() * 180.0 / PI
                } else {
                    (i as f64).atan()
                })),
                Float(f) => res.push(Parameters::Float(if degrees {
                    f.atan() * 180.0 / PI
                } else {
                    f.atan()
                })),
                Rational(s) => res.push(Parameters::Float(if degrees {
                    s.approx().atan() * 180.0 / PI
                } else {
                    s.approx().atan()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(atan(&[s.clone(), Bool(false)], ram))
                            } else {
                                res.push(atan(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("atan".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("atan".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => {
                    if degrees {
                        atan(&[t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        atan(from_ref(t), ram)
                    }
                }
            },
        },
        p => Call("atan".to_string(), Box::from(p.clone())),
    }
}

/// # Exp
/// Computes the exponential of the input
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the exponential of the input argument
///
/// Requirements: at least one argument
/// - If there is a second argument, it modifies the base of the exponential, by default it is the natural base
/// - The first argument must be either an integer, a float, a rational number, or a vector of these arguments.
pub fn exp(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut plus = false;
    let mut ln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Float(f) => ln = *f,
                    Int(i) => ln = (*i) as f64,
                    _ => ln = 0.0,
                }
            }
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Float(ln.powf(fs))
            } else {
                Float(fs.exp())
            }
        }
        Float(f) => {
            if plus {
                Float(ln.powf(*f))
            } else {
                Float((*f).exp())
            }
        }
        Rational(s) => {
            if plus {
                Float(ln.powf((*s).approx()))
            } else {
                Float((*s).approx().exp())
            }
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if plus {
                    ln.powf(i as f64)
                } else {
                    (i as f64).exp()
                })),
                Float(f) => res.push(Float(if plus { ln.powf(f) } else { f.exp() })),
                Rational(s) => res.push(Parameters::Float(if plus {
                    ln.powf(s.approx())
                } else {
                    s.approx().exp()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(exp(&[s.clone(), Float(ln)], ram))
                            } else {
                                res.push(exp(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("exp".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("exp".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => exp(&[t.clone(), Float(ln)], ram),
            },
        },
        p => Call("exp".to_string(), Box::from(p.clone())),
    }
}

/// # Logarithm
/// Inverse of the previous (exponential) function, parameters works the same way
pub fn ln(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut plus = false;
    let mut sln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Float(f) => sln = *f,
                    Int(i) => sln = (*i) as f64,
                    _ => sln = 0.0,
                }
            }
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Float(fs.log(sln))
            } else {
                Float(fs.ln())
            }
        }
        Float(f) => {
            if plus {
                Float((*f).log(sln))
            } else {
                Float((*f).ln())
            }
        }

        Rational(s) => {
            if plus {
                Float((*s).approx().log(sln))
            } else {
                Float((*s).approx().ln())
            }
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if plus {
                    (i as f64).log(sln)
                } else {
                    (i as f64).ln()
                })),
                Float(f) => res.push(Float(if plus { f.log(sln) } else { f.ln() })),
                Rational(s) => res.push(Parameters::Float(if plus {
                    s.approx().log(sln)
                } else {
                    s.approx().ln()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(ln(&[s.clone(), Float(sln)], ram))
                            } else {
                                res.push(ln(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("ln".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("ln".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => ln(&[t.clone(), Float(sln)], ram),
            },
        },
        p => Call("ln".to_string(), Box::from(p.clone())),
    }
}

/// # Root
/// Computes the nth (default is two) root of the input argument
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the nth root of the input argument
///
/// Requirements: at least one argument
/// - If there is a second argument, it modifies the base of the root (default is square root)
/// - The first argument must be either an integer, a float, a rational number, or a vector of these arguments.
pub fn sqrt(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut plus = false;
    let mut sln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Float(f) => sln = *f,
                    Int(i) => sln = (*i) as f64,
                    _ => sln = 0.0,
                }
            }
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Float(fs.powf(1.0 / sln))
            } else {
                Float(fs.sqrt())
            }
        }
        Float(f) => {
            if plus {
                Float((*f).powf(1.0 / sln))
            } else {
                Float((*f).sqrt())
            }
        }
        Rational(s) => {
            if plus {
                Float((*s).approx().powf(1.0 / sln))
            } else {
                Float((*s).approx().sqrt())
            }
        }

        InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Int(i) => res.push(Parameters::Float(if plus {
                    (i as f64).powf(1.0 / sln)
                } else {
                    (i as f64).sqrt()
                })),
                Float(f) => res.push(Parameters::Float(if plus {
                    f.powf(1.0 / sln)
                } else {
                    f.sqrt()
                })),
                Rational(s) => res.push(Parameters::Float(if plus {
                    s.approx().powf(1.0 / sln)
                } else {
                    s.approx().sqrt()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(sqrt(&[s.clone(), Float(sln)], ram))
                            } else {
                                res.push(sqrt(from_ref(s), ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(res)
        }
        Identifier(s) => match ram {
            None => Call("sqrt".to_string(), Box::from(Identifier(s.clone()))),
            Some(ref t) => match t.get(s.as_str()) {
                None => Call("sqrt".to_string(), Box::from(Identifier(s.clone()))),
                Some(t) => sqrt(&[t.clone(), Float(sln)], ram),
            },
        },
        p => Call("sqrt".to_string(), Box::from(p.clone())),
    }
}

/// # Fact
/// Helper function to computes n factorial
/// Takes an integer
/// Returns the factorial of the input.
pub fn fact(n: i64) -> i64 {
    fn aux(n: i64, acc: i64) -> i64 {
        match n {
            0 => acc,
            i => aux(i - 1, i * acc),
        }
    }
    aux(n, 1)
}

/// # Factorial
/// Computes the factorial of the input argument
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram (see above)
/// Returns the factorial of the input argument.
///
/// Requirements: one argument
/// - The argument must be either an integer or a float (in this case, this function computes the factorial of the floor of the float).
pub fn factorial(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int(fact(*i)),
        Float(f) => Parameters::Int(fact(*f as i64)),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => factorial(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Absolute value
/// Computes the absolute value of the input argument
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the absolute value of the argument.
///
/// Requirements: one argument
/// - The argument must be either an integer, a float, or a rational number.
pub fn abs(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int(i.abs()),
        Float(f) => Parameters::Float(f.abs()),
        Rational(s) => Parameters::Rational((*s).abs()),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => abs(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Ceil
/// Computes the ceil of the input argument
/// Takes a reference to the list of parameters
/// Takes a reference to the Ram
/// Returns the ceil of the input argument
///
/// Requirements: one argument
/// - The argument must be an integer or a float.
pub fn ceil(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Float((*i as f64).ceil()),
        Float(f) => Parameters::Float(f.ceil()),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => ceil(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Floor
/// Computes the floor of the input argument
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the floor of the input argument
///
/// Requirements: one argument
/// - The argument must be an integer or a float.
pub fn floor(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Float((*i as f64).floor()),
        Float(f) => Parameters::Float(f.floor()),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => floor(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Round
/// Rounds the input argument
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the rounded value of the argument.
///
/// Requirements: at least one argument
/// - If there is a second argument, the second one will be the number of decimal digits to be rounded to (default is 0)
/// - The first argument must be either an integer, a float, or a rational number.
pub fn round(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    let mut plus = false;
    let mut sln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Float(f) => sln = *f,
                    Int(i) => sln = (*i) as f64,
                    _ => sln = 0.0,
                }
            }
        }
    }

    match p.first().unwrap() {
        Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Float(((fs * 10.0_f64.powf(sln)).round()) / (10.0_f64.powf(sln)))
            } else {
                Float(fs.round())
            }
        }
        Float(f) => {
            if plus {
                Float(((f * 10.0_f64.powf(sln)).round()) / (10.0_f64.powf(sln)))
            } else {
                Float((*f).round())
            }
        }
        Rational(s) => {
            if plus {
                Float(((*s).approx() * 10.0_f64.powf(sln).round()) / (10.0_f64.powf(sln)))
            } else {
                Float((*s).approx().round())
            }
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => round(&[t.clone(), Float(sln)], ram),
            },
        },
        _ => Null,
    }
}

/// # Norm
/// Computes the norm of a vector of arguments
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the norm of the input.
///
/// Requirements: one argument
/// - If the argument is an integer or a float, it returns the absolute value
/// - The argument must be an integer, a float, a rational number, or a vector of these arguments.
pub fn norm(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        InterpreterVector(lst) => {
            let mut sum = Int(0);

            (*lst)
                .iter()
                .map(|x| mult(x.clone(), x.clone(), ram.as_deref()))
                .for_each(|x| sum = other_add(sum.clone(), x.clone(), ram.as_deref()));

            match sum {
                Int(i) => Parameters::Float((i as f64).sqrt()),
                Float(f) => Parameters::Float(f.sqrt()),
                Rational(s) => Parameters::Float(s.approx().sqrt()),
                _ => Float(0.0),
            }
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => norm(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Transpose (Vectors)
/// Computes the transposition of a vector of arguments
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the transposition of the input argument
///
/// Requirements: one argument
/// - The argument must be an integer, a float, a rational number, or a vector of those arguments
/// - If the argument is not a vector, it computes the absolute value.
pub fn transpose_vectors(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational((*s).abs()),
        InterpreterVector(lst) => {
            let r = vec![(lst.clone())];
            let transposed = transpose(r);

            let mut result = Vec::new();

            transposed
                .into_iter()
                .map(InterpreterVector)
                .for_each(|v| result.push(v));

            InterpreterVector(result)
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => transpose_vectors(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Transpose (Matrices)
/// Computes the transposition of a matrix of arguments
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the transposition of the input.
///
/// Requirements: one argument
/// - The argument must be an integer, a float, a rational number, a vector of these arguments, or a matrix of those arguments
/// - If the argument is not a matrix, it uses the vector version of the transposition (see above).
pub fn transpose_matrices(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational((*s).abs()),
        InterpreterVector(lst) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            lst.clone().into_iter().for_each(|x| match x {
                InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });

            if !is_matrix {
                return transpose_vectors(p, ram);
            }

            let matrix_result = transpose(res);
            let mut result = Vec::new();

            matrix_result
                .into_iter()
                .for_each(|x| result.push(InterpreterVector(x)));
            InterpreterVector(result)
        }

        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => transpose_matrices(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Determinant (Matrices)
/// Computes the determinant of a matrix of arguments
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the determinant of the input
///
/// Requirements: one argument
/// - The argument must be an integer, a float, a rational number, a vector of these arguments, or a matrix of these arguments
/// - If the argument is not a matrix it computes the absolute value (or returns zero, in case of a vector)
/// - If the matrix is not square it returns zero.
pub fn det_matrix(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational((*s).abs()),
        InterpreterVector(lst) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            lst.clone().into_iter().for_each(|x| match x {
                InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });

            if !is_matrix {
                return Float(0.0);
            }

            let mut p = Vec::new();
            for _ in 0..(res.len() + 1) {
                p.push(Int(0));
            }
            let n = res.len();
            let r = lup_decompose(&mut res, &mut p, n, ram.as_deref());

            match r {
                0 => Int(0),
                _ => {
                    let det = lup_determinant(&mut res, &mut p, n, ram.as_deref());
                    det
                }
            }
        }

        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => det_matrix(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Inverse (Matrices)
/// Inverts a matrix of arguments
/// Takes a reference to the list of arguments
/// Takes a reference to the Ram
/// Returns the inverted matrix of the input matrix
///
/// Requiremens: one argument
/// - The argument must be an integer, a float, a rational number, a vector of these arguments, or a matrix of these arguments
/// - If the argument is not a matrix it returns the absolute value (or itself if it is a vector)
/// - If the matrix is not square it returns an error
/// - If the matrix is not invertible it returns an error.
pub fn inverse_matrix(p: &[Parameters], ram: &Ram) -> Parameters {
    if p.is_empty() {
        return Null;
    }

    match p.first().unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational((*s).abs()),
        InterpreterVector(lst) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            lst.clone().into_iter().for_each(|x| match x {
                InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });

            if !is_matrix {
                return InterpreterVector(res1);
            }

            let mut p = Vec::new();
            for _ in 0..(res.len() + 1) {
                p.push(Int(0));
            }
            let n = res.len();
            let r = lup_decompose(&mut res, &mut p, n, ram.as_deref());

            match r {
                0 => Identifier("@Determinant is zero, matrice is not invertible".to_string()),
                _ => {
                    let mut vec_ia = Vec::new();
                    for _ in 0..n {
                        let mut vec = Vec::new();
                        for _ in 0..n {
                            vec.push(Int(0));
                        }
                        vec_ia.push(vec);
                    }
                    let det = lup_determinant(&mut res, &mut p, n, ram.as_deref());
                    match det {
                        Int(0) => {
                            return Identifier(
                                "@Determinant is zero, matrix is not invertible".to_string(),
                            )
                        }
                        Float(s) if s.abs() < 1e-10 => {
                            return Identifier(
                                "@Determinant is zero, matrix is not invertible".to_string(),
                            )
                        }
                        Rational(s) if s.is_null() => {
                            return Identifier(
                                "@Determinant is zero, matrix is not invertible".to_string(),
                            )
                        }
                        _ => (),
                    }
                    lup_invert(&mut res, &mut p, n, &mut vec_ia, ram.as_deref());
                    let mut resd = Vec::new();
                    for i in vec_ia.iter().take(n) {
                        resd.push(InterpreterVector(i.clone()));
                    }
                    InterpreterVector(resd)
                }
            }
        }

        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => inverse_matrix(from_ref(t), ram),
            },
        },
        _ => Null,
    }
}

/// # Differenciation
pub fn diff(p: &[Parameters], ram: &Ram, function: &Functions) -> Parameters {
    let color = match load() {
        Ok(cfg) => load_config(cfg).general_color,
        Err(_) => load_config(Config::default()).general_color,
    };

    if p.is_empty() {
        let m = color.paint("Usage: diff <function>");
        println!("{m}");
        return Null;
    }

    let first_param = p.first().unwrap();
    let second_param = p.len() > 1;

    let mut c: HashMap<String, Parameters> = HashMap::new();
    for (key, ele) in ram.as_deref().unwrap().clone() {
        c.insert(key, ele);
    }
    let mut s: HashMap<String, (Vec<Ast>, Ast)> = HashMap::new();
    for (key, ele) in function.as_deref().unwrap().clone() {
        s.insert(key, ele);
    }
    let insert = if second_param {
        p.get(1).unwrap().clone()
    } else {
        Var(Box::from(Int(1)), 1, "x".to_string())
    };
    match first_param {
        Identifier(fun) => match fun.as_str() {
            "cos" => mult(
                Int(-1),
                Call("sin".to_string(), Box::from(insert)),
                Some(&c),
            ),
            "sin" => Call("cos".to_string(), Box::from(insert)),
            "exp" => Call("exp".to_string(), Box::from(insert)),
            "ln" => divide(Int(1), insert, Some(&c)),
            "tan" => divide(
                Int(1),
                mult(
                    Call("cos".to_string(), Box::from(insert.clone())),
                    Call("cos".to_string(), Box::from(insert.clone())),
                    Some(&c),
                ),
                Some(&c),
            ),
            "sinh" => Call("cosh".to_string(), Box::from(insert)),
            "cosh" => Call("sinh".to_string(), Box::from(insert)),
            "acos" => divide(
                Int(-1),
                Call(
                    "sqrt".to_string(),
                    Box::from(minus(
                        Int(1),
                        mult(insert.clone(), insert.clone(), Some(&c)),
                        Some(&c),
                    )),
                ),
                Some(&c),
            ),
            "asin" => divide(
                Int(1),
                Call(
                    "sqrt".to_string(),
                    Box::from(minus(
                        Int(1),
                        mult(insert.clone(), insert.clone(), Some(&c)),
                        Some(&c),
                    )),
                ),
                Some(&c),
            ),
            "x" => Identifier("1".to_string()),
            "sqrt" => divide(
                Int(1),
                mult(
                    Int(2),
                    Call("sqrt".to_string(), Box::from(insert)),
                    Some(&c),
                ),
                Some(&c),
            ),
            p => {
                let param = exec(
                    p.to_string(),
                    vec![Identifier("x".to_string())],
                    Some(&mut c),
                    Some(&mut s),
                );
                match param {
                    Identifier(_) => Int(1),
                    Var(x, y, z) => Var(
                        Box::from(mult(Parameters::Int(y), *x.clone(), Some(&c))),
                        y - 1,
                        z,
                    ),

                    Plus(x, y) => other_add(
                        diff(&[*x.clone()], &Some(&mut c), &Some(&mut s)),
                        diff(&[*y.clone()], &Some(&mut c), &Some(&mut s)),
                        Some(&c),
                    ),
                    Mul(x, y) => other_add(
                        mult(
                            *x.clone(),
                            diff(&[*y.clone()], &Some(&mut c), &Some(&mut s)),
                            Some(&c),
                        ),
                        mult(
                            *y.clone(),
                            diff(&[*x.clone()], &Some(&mut c), &Some(&mut s)),
                            Some(&c),
                        ),
                        Some(&c),
                    ),
                    Div(x, y) => Div(
                        Box::from(other_add(
                            mult(
                                *x.clone(),
                                diff(&[*y.clone()], &Some(&mut c), &Some(&mut s)),
                                Some(&c),
                            ),
                            mult(
                                mult(Int(-1), *y.clone(), Some(&c)),
                                diff(&[*x.clone()], &Some(&mut c), &Some(&mut s)),
                                Some(&c),
                            ),
                            Some(&c),
                        )),
                        Box::from(mult(*y.clone(), *y.clone(), Some(&c))),
                    ),
                    Call(name, pst) => {
                        let prefix = diff(&[*pst.clone()], &Some(&mut c), &Some(&mut s));
                        let call = diff(
                            &[Identifier(name), *pst.clone()],
                            &Some(&mut c),
                            &Some(&mut s),
                        );
                        mult(prefix, call, Some(&c))
                    }
                    _ => Int(0),
                }
            }
        },
        Var(x, y, z) => Var(
            Box::from(mult(Parameters::Int(*y), *x.clone(), Some(&c))),
            y - 1,
            z.clone(),
        ),
        Plus(x, y) => other_add(
            diff(&[*x.clone()], &Some(&mut c), &Some(&mut s)),
            diff(&[*y.clone()], &Some(&mut c), &Some(&mut s)),
            Some(&c),
        ),
        Mul(x, y) => other_add(
            mult(
                *x.clone(),
                diff(&[*y.clone()], &Some(&mut c), &Some(&mut s)),
                Some(&c),
            ),
            mult(
                *y.clone(),
                diff(&[*x.clone()], &Some(&mut c), &Some(&mut s)),
                Some(&c),
            ),
            Some(&c),
        ),
        Div(x, y) => Div(
            Box::from(other_add(
                mult(
                    *x.clone(),
                    diff(&[*y.clone()], &Some(&mut c), &Some(&mut s)),
                    Some(&c),
                ),
                mult(
                    Mul(Box::from(Int(-1)), y.clone()),
                    diff(&[*x.clone()], &Some(&mut c), &Some(&mut s)),
                    Some(&c),
                ),
                Some(&c),
            )),
            Box::from(mult(*y.clone(), *y.clone(), Some(&c))),
        ),

        Call(name, pst) => {
            let prefix = diff(&[*pst.clone()], &Some(&mut c), &Some(&mut s));
            let call = diff(
                &[Identifier(name.to_string()), *pst.clone()],
                &Some(&mut c),
                &Some(&mut s),
            );
            mult(prefix, call, Some(&c))
        }
        _ => Int(0),
    }
}

pub fn plot_fn(p: &[Parameters], ram: &Ram, functions: &Functions, terminal: bool) -> Parameters {
    let color = match load() {
        Ok(cfg) => load_config(cfg).general_color,
        Err(_) => load_config(Config::default()).general_color,
    };

    if p.is_empty() {
        let m = color.paint(" > plot(): displays help\n > plot(f): plot f\n > plot(f,title,xlabel,ylabel): plot f with title,xlabel,ylabel\n > plot(f,mode): plot f with the mode=LINE|LINEMARKS|MARKS(default)\n > plot(f,title,xlabel,ylabel,mode): plot f with title,xlabel,ylabel and mode\n > plot(f,start,end,step,mode): plot f between start and end with steps and mode\n > plot(f,start,end,step,title,xlabel,ylabel,mode): combines\n");
        println!("{m}");
        return Null;
    }

    let fs = p.first().unwrap();
    let mut f: fn(&[Parameters], &Ram) -> Parameters = cos;
    let mut fd: String = "".to_string();
    let mut rad: bool = false;
    let mut fun: bool = true;
    let mut first_vector = None;
    let mut second_vector = None;
    match fs {
        InterpreterVector(vec) => {
            fun = false;
            first_vector = Some(&**vec)
        }
        Identifier(s) => match s.as_str() {
            "cos" => {
                f = cos;
                rad = true
            }
            "sin" => {
                f = sin;
                rad = true
            }
            "tan" => {
                f = tan;
                rad = true
            }
            "cosh" => {
                f = cosh;
                rad = true
            }
            "sinh" => {
                f = sinh;
                rad = true
            }
            "tanh" => {
                f = tanh;
                rad = true
            }
            "exp" => f = exp,
            "acos" => f = acos,
            "asin" => f = asin,
            "atan" => f = atan,
            "ln" => f = ln,
            "log" => f = ln,
            "sqrt" => f = sqrt,
            s => match functions {
                None => match ram.as_ref().unwrap().get(s) {
                    None => return Null,
                    Some(InterpreterVector(vec)) => {
                        fun = false;
                        first_vector = Some(&**vec);
                    }
                    _ => return Null,
                },
                Some(ref t) => {
                    if t.contains_key(s) {
                        fd = s.to_string();
                    } else {
                        match ram.as_ref().unwrap().get(s) {
                            None => return Null,
                            Some(InterpreterVector(vec)) => {
                                fun = false;
                                first_vector = Some(&**vec)
                            }
                            _ => return Null,
                        }
                    }
                }
            },
        },
        _ => return Null,
    }

    let mut start = 0.0;
    let mut end = 10.0;
    let mut steps = 0.01;
    let mut title = "".to_string();
    let mut xlabel = "".to_string();
    let mut ylabel = "".to_string();
    let mut mode = "marks";

    if rad {
        end = 3.0 * PI;
        steps = 0.01 * PI;
    }
    match p.get(1) {
        None => (),
        Some(p) => match p {
            Float(f) => start = *f,
            Int(i) => start = *i as f64,
            Rational(s) => start = (*s).approx(),
            InterpreterVector(vec) => second_vector = Some(&**vec),

            Identifier(s) if ram.as_ref().unwrap().contains_key(s) => {
                match ram.as_ref().unwrap().get(s) {
                    Some(Float(f)) => start = *f,
                    Some(Int(i)) => start = *i as f64,
                    Some(InterpreterVector(vec)) => second_vector = Some(&**vec),

                    _ => (),
                }
            }
            Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => title = s.to_string(),
            },
            _ => (),
        },
    };

    match p.get(2) {
        None => (),
        Some(p) => match p {
            Float(f) => end = *f,
            Int(i) => end = *i as f64,
            Rational(s) => end = (*s).approx(),

            Identifier(s) if ram.as_ref().unwrap().contains_key(s) => {
                match ram.as_ref().unwrap().get(s) {
                    Some(Float(f)) => {
                        end = *f;
                    }
                    Some(Int(i)) => end = *i as f64,
                    _ => (),
                }
            }
            Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == *"" {
                        title = s.to_string()
                    } else {
                        xlabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
    }

    match p.get(3) {
        None => (),
        Some(p) => match p {
            Float(f) => steps = *f,
            Int(i) => steps = *i as f64,
            Rational(s) => steps = (*s).approx(),

            Identifier(s) if ram.as_ref().unwrap().contains_key(s) => {
                match ram.as_ref().unwrap().get(s) {
                    Some(Float(f)) => steps = *f,
                    Some(Int(i)) => steps = *i as f64,
                    _ => (),
                }
            }
            Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == *"" {
                        title = s.to_string()
                    } else if xlabel == *"" {
                        xlabel = s.to_string()
                    } else {
                        ylabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
    }

    if let Some(Str(s)) = p.get(4) {
        match s.to_lowercase().as_str() {
            "marks" => mode = "marks",
            "line" => mode = "line",
            "linemarks" => mode = "linemarks",
            _ => {
                if title == *"" {
                    title = s.to_string()
                } else if xlabel == *"" {
                    xlabel = s.to_string()
                } else {
                    ylabel = s.to_string()
                }
            }
        }
    }

    if let Some(Str(s)) = p.get(5) {
        match s.to_lowercase().as_str() {
            "marks" => mode = "marks",
            "line" => mode = "line",
            "linemarks" => mode = "linemarks",
            _ => {
                if title == *"" {
                    title = s.to_string()
                } else if xlabel == *"" {
                    xlabel = s.to_string()
                } else {
                    ylabel = s.to_string()
                }
            }
        }
    }

    if let Some(Str(s)) = p.get(6) {
        match s.to_lowercase().as_str() {
            "marks" => mode = "marks",
            "line" => mode = "line",
            "linemarks" => mode = "linemarks",
            _ => {
                if title == *"" {
                    title = s.to_string()
                } else if xlabel == *"" {
                    xlabel = s.to_string()
                } else {
                    ylabel = s.to_string()
                }
            }
        }
    }

    if let Some(Str(s)) = p.get(7) {
        match s.to_lowercase().as_str() {
            "marks" => mode = "marks",
            "line" => mode = "line",
            "linemarks" => mode = "linemarks",
            _ => {
                if title == *"" {
                    title = s.to_string()
                } else if xlabel == *"" {
                    xlabel = s.to_string()
                } else if ylabel == *"" {
                    ylabel = s.to_string()
                }
            }
        }
    }

    let st = start;
    let mut x = Vec::new();
    let mut y = Vec::new();
    if fun {
        let (mut vec, mut ast): (Vec<Ast>, Ast) = (Vec::new(), Ast::Nil);
        match functions {
            None => (),
            Some(ref s) => {
                if s.contains_key(&fd) {
                    (vec, ast) = s.get(&fd).unwrap().clone();
                }
            }
        }

        let mut sram: HashMap<String, Parameters> = HashMap::new();
        sram.insert("pi".to_string(), Float(PI));
        sram.insert("e".to_string(), Float(E));
        while start <= end {
            x.push(start);
            if fd.is_empty() {
                let p = f(&[Float(start)], ram);
                y.push(match p {
                    Float(f) => f,
                    Int(i) => i as f64,
                    Rational(s) => s.approx(),
                    _ => f64::NAN,
                });
            } else {
                let mut names = Vec::new();
                for v in vec.clone() {
                    match v {
                        Ast::Nil => (),
                        Ast::Call { .. } => (),
                        Ast::Conditional { .. } => (),
                        Ast::While { .. } => (),
                        Ast::Ignore { .. } => (),
                        Ast::Node {
                            value: v,
                            left: _l,
                            right: _r,
                        } => {
                            if let Identifier(s) = v {
                                names.push(s.clone())
                            }
                        }
                    }
                }
                names
                    .iter()
                    .zip(vec![Float(start)])
                    .for_each(|(name, param)| {
                        sram.insert(name.to_string(), param.clone());
                    });
                y.push(match interpret(&ast, &mut sram, &mut HashMap::new()) {
                    Float(p) => p,
                    Int(i) => i as f64,
                    Rational(s) => s.approx(),
                    _ => f64::NAN,
                });
            }
            start += steps;
        }
    } else {
        match first_vector {
            Some(t) => {
                t.iter().for_each(|j| match j {
                    Int(i) => x.push(*i as f64),
                    Float(f) => x.push(*f),
                    Rational(s) => x.push((*s).approx()),
                    Identifier(s) => match ram.as_ref().unwrap().get(s) {
                        Some(Int(i)) => x.push(*i as f64),
                        Some(Float(f)) => x.push(*f),
                        Some(Rational(r)) => x.push((*r).approx()),
                        _ => (),
                    },
                    _ => (),
                });
            }
            _ => return Null,
        }

        match second_vector {
            Some(t) => {
                t.iter().for_each(|j| match j {
                    Int(i) => y.push(*i as f64),
                    Float(f) => y.push(*f),
                    Rational(r) => y.push((*r).approx()),
                    Identifier(s) => match ram.as_ref().unwrap().get(s) {
                        Some(Int(i)) => y.push(*i as f64),
                        Some(Float(f)) => y.push(*f),
                        Some(Rational(r)) => y.push((*r).approx()),
                        _ => (),
                    },
                    _ => (),
                });
            }
            _ => return Null,
        }
    }
    let mut f: Figure = Figure::new();
    let _ = match mode.to_lowercase().as_str() {
        "marks" => f
            .axes2d()
            .set_x_label(&xlabel, &[])
            .set_y_label(&ylabel, &[])
            .set_title(&title, &[])
            .points(&x, &y, &[]),
        "line" => f
            .axes2d()
            .set_x_label(&xlabel, &[])
            .set_y_label(&ylabel, &[])
            .set_title(&title, &[])
            .lines(&x, &y, &[]),
        "linemarks" => f
            .axes2d()
            .set_x_label(&xlabel, &[])
            .set_y_label(&ylabel, &[])
            .set_title(&title, &[])
            .lines_points(&x, &y, &[]),
        _ => f.axes2d().points(&x, &y, &[]),
    };
    if !terminal {
        f.show().unwrap();
    } else {
        computes_lines(&x, &y, st, end, title, xlabel, ylabel)
    }
    Null
}
