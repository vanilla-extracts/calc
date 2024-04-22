use std::collections::HashMap;
use std::f64::consts::{E, PI};

use gnuplot::{AxesCommon, Figure};

use crate::configuration::loader::{load, load_config, Config};
use crate::interpreting::interpreter::interpret;
use crate::parsing::ast::{Ast, Parameters};
use crate::utils::matrix_utils::{lup_decompose, lup_determinant, lup_invert, transpose};
use crate::utils::plot_utils::computes_lines;

use super::function::{add as other_add, mult};

pub fn exec(
    s: String,
    lst: Vec<Parameters>,
    ram: Option<&mut HashMap<String, Parameters>>,
    functions: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
) -> Parameters {
    match s.as_str() {
        "cos" => cos(&lst, &ram),
        "sin" => sin(&lst, &ram),
        "tan" => tan(&lst, &ram),
        "cosh" => cosh(&lst, &ram),
        "sinh" => sinh(&lst, &ram),
        "tanh" => tanh(&lst, &ram),
        "exp" => exp(&lst, &ram),
        "acos" => acos(&lst, &ram),
        "asin" => asin(&lst, &ram),
        "atan" => atan(&lst, &ram),
        "ln" => ln(&lst, &ram),
        "log" => ln(&lst, &ram),
        "sqrt" => sqrt(&lst, &ram),
        "fact" => factorial(&lst, &ram),
        "factorial" => factorial(&lst, &ram),
        "abs" => abs(&lst, &ram),
        "ceil" => ceil(&lst, &ram),
        "floor" => floor(&lst, &ram),
        "round" => round(&lst, &ram),
        "norm" => norm(&lst, &ram, functions),
        "transpose_vector" => transpose_vectors(&lst, &ram),
        "transpose" => transpose_matrices(&lst, &ram),
        "det" => det_matrix(&lst, &ram),
        "invert" => inverse_matrix(&lst, &ram),
        "plot" => plot_fn(&lst, &ram, functions, false),
        "termplot" => plot_fn(&lst, &ram, functions, true),
        s => {
            let mut sram: HashMap<String, Parameters> = HashMap::new();
            sram.insert("pi".to_string(), Parameters::Float(PI));
            sram.insert("e".to_string(), Parameters::Float(E));
            match functions.cloned() {
                None => Parameters::Identifier("This function is unknown".to_string()),
                Some(mut f) => {
                    let fs = f.get_mut(s);
                    let (vec, ast): (&mut Vec<Ast>, &mut Ast) = match fs {
                        None => {
                            return Parameters::Identifier("This function is unknown".to_string());
                        }
                        Some((a, b)) => (a, b),
                    };

                    let mut names = Vec::new();
                    for v in vec {
                        match v {
                            Ast::Nil => (),
                            Ast::Call { .. } => (),
                            Ast::Node {
                                value: v,
                                left: _l,
                                right: _r,
                            } => match v {
                                Parameters::Identifier(s) => names.push(s.clone()),
                                _ => (),
                            },
                        }
                    }
                    names.iter().zip(lst).for_each(|(name, param)| {
                        sram.insert(name.to_string(), param);
                    });
                    interpret(ast, &mut sram, &mut HashMap::new())
                }
            }
        }
    }
}

pub fn cos(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Parameters::Float(fs.cos())
        }
        Parameters::Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Parameters::Float(fs.cos())
        }
        Parameters::Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
            };
            Parameters::Float(fs.cos())
        }
        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).cos()
                } else {
                    (i as f64).cos()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).cos()
                } else {
                    f.cos()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).cos()
                } else {
                    s.approx().cos()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(cos(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(cos(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        cos(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        cos(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn sin(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Parameters::Float(fs.sin())
        }
        Parameters::Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Parameters::Float(fs.sin())
        }
        Parameters::Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
            };
            Parameters::Float(fs.sin())
        }
        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).sin()
                } else {
                    (i as f64).sin()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).sin()
                } else {
                    f.sin()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).sin()
                } else {
                    s.approx().sin()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(sin(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(sin(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        sin(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        sin(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn tan(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Parameters::Float(fs.tan())
        }
        Parameters::Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Parameters::Float(fs.tan())
        }
        Parameters::Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
            };
            Parameters::Float(fs.tan())
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).tan()
                } else {
                    (i as f64).tan()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).tan()
                } else {
                    f.tan()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).tan()
                } else {
                    s.approx().tan()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(tan(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(tan(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        tan(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        tan(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn cosh(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Parameters::Float(fs.cosh())
        }
        Parameters::Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Parameters::Float(fs.cosh())
        }
        Parameters::Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
            };
            Parameters::Float(fs.cosh())
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).cosh()
                } else {
                    (i as f64).cosh()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).cosh()
                } else {
                    f.cosh()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).cosh()
                } else {
                    s.approx().cosh()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(cosh(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(cosh(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        cosh(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        cosh(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn sinh(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Parameters::Float(fs.sinh())
        }
        Parameters::Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Parameters::Float(fs.sinh())
        }
        Parameters::Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
            };
            Parameters::Float(fs.sinh())
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).sinh()
                } else {
                    (i as f64).sinh()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).sinh()
                } else {
                    f.sinh()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).sinh()
                } else {
                    s.approx().sinh()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(sinh(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(sinh(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        sinh(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        sinh(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn tanh(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Parameters::Float(fs.tanh())
        }
        Parameters::Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Parameters::Float(fs.tanh())
        }
        Parameters::Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
            };
            Parameters::Float(fs.tanh())
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    ((i as f64) * PI / 180.0).tanh()
                } else {
                    (i as f64).tanh()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    (f * PI / 180.0).tanh()
                } else {
                    f.tanh()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    (s.approx() * PI / 180.0).tanh()
                } else {
                    s.approx().tanh()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(tanh(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(tanh(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        tanh(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        tanh(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn acos(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            Parameters::Float(if degrees {
                fs.acos() * (180.0 / PI)
            } else {
                fs.acos()
            })
        }
        Parameters::Float(f) => Parameters::Float(if degrees {
            f.acos() * (180.0 / PI)
        } else {
            f.acos()
        }),
        Parameters::Rational(s) => Parameters::Float(if degrees {
            s.clone().approx().acos() * 180.0 / PI
        } else {
            s.clone().approx().acos()
        }),

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    (i as f64).acos() * 180.0 / PI
                } else {
                    (i as f64).acos()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    f.acos() * 180.0 / PI
                } else {
                    f.acos()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    s.approx().acos() * 180.0 / PI
                } else {
                    s.approx().acos()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(acos(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(acos(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        acos(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        acos(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn asin(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            Parameters::Float(if degrees {
                fs.asin() * (180.0 / PI)
            } else {
                fs.asin()
            })
        }
        Parameters::Float(f) => Parameters::Float(if degrees {
            f.asin() * (180.0 / PI)
        } else {
            f.asin()
        }),

        Parameters::Rational(s) => Parameters::Float(if degrees {
            s.clone().approx().asin() * (180.0 / PI)
        } else {
            s.clone().approx().asin()
        }),

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    (i as f64).asin() * 180.0 / PI
                } else {
                    (i as f64).asin()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    f.asin() * 180.0 / PI
                } else {
                    f.asin()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    s.approx().asin() * 180.0 / PI
                } else {
                    s.approx().asin()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(asin(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(asin(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        asin(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        asin(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn atan(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            Parameters::Float(if degrees {
                fs.atan() * (180.0 / PI)
            } else {
                fs.atan()
            })
        }
        Parameters::Float(f) => Parameters::Float(if degrees {
            f.atan() * (180.0 / PI)
        } else {
            f.atan()
        }),

        Parameters::Rational(s) => Parameters::Float(if degrees {
            s.clone().approx().atan() * (180.0 / PI)
        } else {
            s.clone().approx().atan()
        }),

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if degrees {
                    (i as f64).atan() * 180.0 / PI
                } else {
                    (i as f64).atan()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if degrees {
                    f.atan() * 180.0 / PI
                } else {
                    f.atan()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if degrees {
                    s.approx().atan() * 180.0 / PI
                } else {
                    s.approx().atan()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if degrees {
                                res.push(atan(&vec![s.clone(), Parameters::Bool(false)], ram))
                            } else {
                                res.push(atan(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => {
                    if degrees {
                        atan(
                            &vec![t.clone(), Parameters::Identifier("false".to_string())],
                            ram,
                        )
                    } else {
                        atan(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Parameters::Null,
    }
}

pub fn exp(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut plus = false;
    let mut ln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Parameters::Float(f) => ln = *f,
                    Parameters::Int(i) => ln = (*i) as f64,
                    _ => ln = 0.0,
                }
            }
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Parameters::Float(ln.powf(fs))
            } else {
                Parameters::Float(fs.exp())
            }
        }
        Parameters::Float(f) => {
            if plus {
                Parameters::Float(ln.powf(*f))
            } else {
                Parameters::Float((*f).exp())
            }
        }
        Parameters::Rational(s) => {
            if plus {
                Parameters::Float(ln.powf(s.clone().approx()))
            } else {
                Parameters::Float(s.clone().approx().exp())
            }
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if plus {
                    ln.powf(i as f64)
                } else {
                    (i as f64).exp()
                })),
                Parameters::Float(f) => {
                    res.push(Parameters::Float(if plus { ln.powf(f) } else { f.exp() }))
                }
                Parameters::Rational(s) => res.push(Parameters::Float(if plus {
                    ln.powf(s.approx())
                } else {
                    s.approx().exp()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(exp(&vec![s.clone(), Parameters::Float(ln)], ram))
                            } else {
                                res.push(exp(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => exp(&vec![t.clone(), Parameters::Float(ln)], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn ln(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut plus = false;
    let mut sln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Parameters::Float(f) => sln = *f,
                    Parameters::Int(i) => sln = (*i) as f64,
                    _ => sln = 0.0,
                }
            }
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Parameters::Float(fs.log(sln))
            } else {
                Parameters::Float(fs.ln())
            }
        }
        Parameters::Float(f) => {
            if plus {
                Parameters::Float((*f).log(sln))
            } else {
                Parameters::Float((*f).ln())
            }
        }

        Parameters::Rational(s) => {
            if plus {
                Parameters::Float(s.clone().approx().log(sln))
            } else {
                Parameters::Float(s.clone().approx().ln())
            }
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if plus {
                    (i as f64).log(sln)
                } else {
                    (i as f64).ln()
                })),
                Parameters::Float(f) => {
                    res.push(Parameters::Float(if plus { f.log(sln) } else { f.ln() }))
                }
                Parameters::Rational(s) => res.push(Parameters::Float(if plus {
                    s.approx().log(sln)
                } else {
                    s.approx().ln()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(ln(&vec![s.clone(), Parameters::Float(sln)], ram))
                            } else {
                                res.push(ln(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => ln(&vec![t.clone(), Parameters::Float(sln)], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn sqrt(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut plus = false;
    let mut sln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Parameters::Float(f) => sln = *f,
                    Parameters::Int(i) => sln = (*i) as f64,
                    _ => sln = 0.0,
                }
            }
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Parameters::Float(fs.powf(1.0 / sln))
            } else {
                Parameters::Float(fs.sqrt())
            }
        }
        Parameters::Float(f) => {
            if plus {
                Parameters::Float((*f).powf(1.0 / sln))
            } else {
                Parameters::Float((*f).sqrt())
            }
        }
        Parameters::Rational(s) => {
            if plus {
                Parameters::Float(s.clone().approx().powf(1.0 / sln))
            } else {
                Parameters::Float(s.clone().approx().sqrt())
            }
        }

        Parameters::InterpreterVector(vec) => {
            let mut res = Vec::new();
            vec.clone().into_iter().for_each(|x| match x {
                Parameters::Int(i) => res.push(Parameters::Float(if plus {
                    (i as f64).powf(1.0 / sln)
                } else {
                    (i as f64).sqrt()
                })),
                Parameters::Float(f) => res.push(Parameters::Float(if plus {
                    f.powf(1.0 / sln)
                } else {
                    f.sqrt()
                })),
                Parameters::Rational(s) => res.push(Parameters::Float(if plus {
                    s.clone().approx().powf(1.0 / sln)
                } else {
                    s.clone().approx().sqrt()
                })),
                Parameters::Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(sqrt(&vec![s.clone(), Parameters::Float(sln)], ram))
                            } else {
                                res.push(sqrt(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            Parameters::InterpreterVector(Box::from(res))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => sqrt(&vec![t.clone(), Parameters::Float(sln)], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn fact(n: i64) -> i64 {
    fn aux(n: i64, acc: i64) -> i64 {
        match n {
            0 => acc,
            i => aux(i - 1, i * acc),
        }
    }
    aux(n, 1)
}

pub fn factorial(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int(fact(*i)),
        Parameters::Float(f) => Parameters::Int(fact(*f as i64)),
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => factorial(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn abs(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int(i.abs()),
        Parameters::Float(f) => Parameters::Float(f.abs()),
        Parameters::Rational(s) => Parameters::Rational(s.clone().abs()),
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => abs(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn ceil(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Float((*i as f64).ceil()),
        Parameters::Float(f) => Parameters::Float(f.ceil()),
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => ceil(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn floor(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Float((*i as f64).floor()),
        Parameters::Float(f) => Parameters::Float(f.floor()),
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => floor(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn round(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    let mut plus = false;
    let mut sln: f64 = 0.0;

    if p.len() > 1 {
        match p.get(1) {
            None => plus = false,
            Some(t) => {
                plus = true;
                match t {
                    Parameters::Float(f) => sln = *f,
                    Parameters::Int(i) => sln = (*i) as f64,
                    _ => sln = 0.0,
                }
            }
        }
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => {
            let fs: f64 = (*i) as f64;
            if plus {
                Parameters::Float(((fs * 10.0_f64.powf(sln)).round()) / (10.0_f64.powf(sln)))
            } else {
                Parameters::Float(fs.round())
            }
        }
        Parameters::Float(f) => {
            if plus {
                Parameters::Float(((f * 10.0_f64.powf(sln)).round()) / (10.0_f64.powf(sln)))
            } else {
                Parameters::Float((*f).round())
            }
        }
        Parameters::Rational(s) => {
            if plus {
                Parameters::Float(
                    (s.clone().approx() * 10.0_f64.powf(sln).round()) / (10.0_f64.powf(sln)),
                )
            } else {
                Parameters::Float(s.clone().approx().round())
            }
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => round(&vec![t.clone(), Parameters::Float(sln)], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn norm(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
    function: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int((*i).abs()),
        Parameters::Float(f) => Parameters::Float((*f).abs()),
        Parameters::InterpreterVector(lst) => {
            let mut sum = Parameters::Int(0);

            (*lst)
                .iter()
                .map(|x| mult(x.clone(), x.clone(), ram.as_deref()))
                .for_each(|x| sum = other_add(sum.clone(), x.clone(), ram.as_deref()));

            match sum {
                Parameters::Int(i) => Parameters::Float((i as f64).sqrt()),
                Parameters::Float(f) => Parameters::Float(f.sqrt()),
                Parameters::Rational(s) => Parameters::Float(s.approx().sqrt()),
                _ => Parameters::Float(0.0),
            }
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => norm(&vec![t.clone()], ram, function),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn transpose_vectors(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int((*i).abs()),
        Parameters::Float(f) => Parameters::Float((*f).abs()),
        Parameters::Rational(s) => Parameters::Rational(s.clone().abs()),
        Parameters::InterpreterVector(lst) => {
            let r = vec![*(lst.clone())];
            let transposed = transpose(r);

            let mut result = Vec::new();

            transposed
                .into_iter()
                .map(|v| Parameters::InterpreterVector(Box::from(v)))
                .for_each(|v| result.push(v));

            Parameters::InterpreterVector(Box::from(result))
        }
        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => transpose_vectors(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn transpose_matrices(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int((*i).abs()),
        Parameters::Float(f) => Parameters::Float((*f).abs()),
        Parameters::Rational(s) => Parameters::Rational(s.clone().abs()),
        Parameters::InterpreterVector(lst) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            lst.clone().into_iter().for_each(|x| match x {
                Parameters::InterpreterVector(l) => res.push(l.to_vec()),
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
                .for_each(|x| result.push(Parameters::InterpreterVector(Box::from(x))));
            Parameters::InterpreterVector(Box::from(result))
        }

        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => transpose_matrices(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn det_matrix(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int((*i).abs()),
        Parameters::Float(f) => Parameters::Float((*f).abs()),
        Parameters::Rational(s) => Parameters::Rational(s.clone().abs()),
        Parameters::InterpreterVector(lst) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            lst.clone().into_iter().for_each(|x| match x {
                Parameters::InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });

            if !is_matrix {
                return Parameters::Float(0.0);
            }

            let mut p = Vec::new();
            for _ in 0..(res.len() + 1) {
                p.push(Parameters::Int(0));
            }
            let n = res.len();
            let r = lup_decompose(&mut res, &mut p, n, ram.as_deref());

            match r {
                0 => Parameters::Int(0),
                _ => {
                    let det = lup_determinant(&mut res, &mut p, n, ram.as_deref());
                    det
                }
            }
        }

        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => det_matrix(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn inverse_matrix(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Parameters::Null;
    }

    match p.get(0).unwrap() {
        Parameters::Int(i) => Parameters::Int((*i).abs()),
        Parameters::Float(f) => Parameters::Float((*f).abs()),
        Parameters::Rational(s) => Parameters::Rational(s.clone().abs()),
        Parameters::InterpreterVector(lst) => {
            let mut res1 = Vec::new();
            let mut is_matrix = true;
            let mut res = Vec::new();
            lst.clone().into_iter().for_each(|x| match x {
                Parameters::InterpreterVector(l) => res.push(l.to_vec()),
                p => {
                    is_matrix = false;
                    res1.push(p);
                }
            });

            if !is_matrix {
                return Parameters::InterpreterVector(Box::from(res1));
            }

            let mut p = Vec::new();
            for _ in 0..(res.len() + 1) {
                p.push(Parameters::Int(0));
            }
            let n = res.len();
            let r = lup_decompose(&mut res, &mut p, n, ram.as_deref());

            match r {
                0 => Parameters::Null,
                _ => {
                    let mut vec_ia = Vec::new();
                    for _ in 0..n {
                        let mut vec = Vec::new();
                        for _ in 0..n {
                            vec.push(Parameters::Int(0));
                        }
                        vec_ia.push(vec);
                    }
                    let det = lup_determinant(&mut res, &mut p, n, ram.as_deref());
                    match det {
                        Parameters::Int(0) => {
                            return Parameters::Str(
                                "Determinant is zero, matrix is not invertible".to_string(),
                            )
                        }
                        Parameters::Float(s) if s.abs() < 1e-10 => {
                            return Parameters::Str(
                                "Determinant is zero, matrix is not invertible".to_string(),
                            )
                        }
                        Parameters::Rational(s) if s.clone().is_null() => {
                            return Parameters::Str(
                                "Determinant is zero, matrix is not invertible".to_string(),
                            )
                        }
                        _ => (),
                    }
                    lup_invert(&mut res, &mut p, n, &mut vec_ia, ram.as_deref());
                    let mut resd = Vec::new();
                    for i in 0..n {
                        resd.push(Parameters::InterpreterVector(Box::new(vec_ia[i].clone())));
                    }
                    Parameters::InterpreterVector(Box::new(resd))
                }
            }
        }

        Parameters::Identifier(s) => match ram {
            None => Parameters::Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Parameters::Null,
                Some(t) => inverse_matrix(&vec![t.clone()], ram),
            },
        },
        _ => Parameters::Null,
    }
}

pub fn plot_fn(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
    functions: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
    terminal: bool,
) -> Parameters {
    let color = match load() {
        Ok(cfg) => load_config(cfg).general_color,
        Err(_) => load_config(Config::default()).general_color,
    };

    if p.len() == 0 {
        let m = color.paint(" > plot(): displays help\n > plot(f): plot f\n > plot(f,title,xlabel,ylabel): plot f with title,xlabel,ylabel\n > plot(f,mode): plot f with the mode=LINE|LINEMARKS|MARKS(default)\n > plot(f,title,xlabel,ylabel,mode): plot f with title,xlabel,ylabel and mode\n > plot(f,start,end,step,mode): plot f between start and end with steps and mode\n > plot(f,start,end,step,title,xlabel,ylabel,mode): combines\n");
        println!("{m}");
        return Parameters::Null;
    }

    let fs = p.first().unwrap();
    let mut f: fn(&Vec<Parameters>, &Option<&mut HashMap<String, Parameters>>) -> Parameters = cos;
    let mut fd: String = "".to_string();
    let mut rad: bool = false;
    let mut fun: bool = true;
    let mut first_vector = None;
    let mut second_vector = None;
    match fs {
        Parameters::InterpreterVector(vec) => {
            fun = false;
            first_vector = Some(&**vec)
        }
        Parameters::Identifier(s) => match s.as_str() {
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
                    None => return Parameters::Null,
                    Some(Parameters::InterpreterVector(vec)) => {
                        fun = false;
                        first_vector = Some(&**vec);
                    }
                    _ => return Parameters::Null,
                },
                Some(ref t) => {
                    if t.contains_key(s) {
                        fd = s.to_string();
                    } else {
                        match ram.as_ref().unwrap().get(s) {
                            None => return Parameters::Null,
                            Some(Parameters::InterpreterVector(vec)) => {
                                fun = false;
                                first_vector = Some(&**vec)
                            }
                            _ => return Parameters::Null,
                        }
                    }
                }
            },
        },
        _ => return Parameters::Null,
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
            Parameters::Float(f) => start = *f,
            Parameters::Int(i) => start = *i as f64,
            Parameters::Rational(s) => start = s.clone().approx(),
            Parameters::InterpreterVector(vec) => second_vector = Some(&**vec),

            Parameters::Identifier(s) if ram.as_ref().unwrap().contains_key(s) => {
                match ram.as_ref().unwrap().get(s) {
                    Some(Parameters::Float(f)) => start = *f,
                    Some(Parameters::Int(i)) => start = *i as f64,
                    Some(Parameters::InterpreterVector(vec)) => second_vector = Some(&**vec),

                    _ => (),
                }
            }
            Parameters::Str(s) => match s.to_lowercase().as_str() {
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
            Parameters::Float(f) => end = *f,
            Parameters::Int(i) => end = *i as f64,
            Parameters::Rational(s) => end = s.clone().approx(),

            Parameters::Identifier(s) if ram.as_ref().unwrap().contains_key(s) => {
                match ram.as_ref().unwrap().get(s) {
                    Some(Parameters::Float(f)) => {
                        end = *f;
                    }
                    Some(Parameters::Int(i)) => end = *i as f64,
                    _ => (),
                }
            }
            Parameters::Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == "".to_string() {
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
            Parameters::Float(f) => steps = *f,
            Parameters::Int(i) => steps = *i as f64,
            Parameters::Rational(s) => steps = s.clone().approx(),

            Parameters::Identifier(s) if ram.as_ref().unwrap().contains_key(s) => {
                match ram.as_ref().unwrap().get(s) {
                    Some(Parameters::Float(f)) => steps = *f,
                    Some(Parameters::Int(i)) => steps = *i as f64,
                    _ => (),
                }
            }
            Parameters::Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == "".to_string() {
                        title = s.to_string()
                    } else if xlabel == "".to_string() {
                        xlabel = s.to_string()
                    } else {
                        ylabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
    }

    match p.get(4) {
        None => (),
        Some(p) => match p {
            Parameters::Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == "".to_string() {
                        title = s.to_string()
                    } else if xlabel == "".to_string() {
                        xlabel = s.to_string()
                    } else {
                        ylabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
    }

    match p.get(5) {
        None => (),
        Some(p) => match p {
            Parameters::Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == "".to_string() {
                        title = s.to_string()
                    } else if xlabel == "".to_string() {
                        xlabel = s.to_string()
                    } else {
                        ylabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
    }

    match p.get(6) {
        None => (),
        Some(p) => match p {
            Parameters::Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == "".to_string() {
                        title = s.to_string()
                    } else if xlabel == "".to_string() {
                        xlabel = s.to_string()
                    } else {
                        ylabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
    }

    match p.get(7) {
        None => (),
        Some(p) => match p {
            Parameters::Str(s) => match s.to_lowercase().as_str() {
                "marks" => mode = "marks",
                "line" => mode = "line",
                "linemarks" => mode = "linemarks",
                _ => {
                    if title == "".to_string() {
                        title = s.to_string()
                    } else if xlabel == "".to_string() {
                        xlabel = s.to_string()
                    } else if ylabel == "".to_string() {
                        ylabel = s.to_string()
                    }
                }
            },
            _ => (),
        },
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
        sram.insert("pi".to_string(), Parameters::Float(PI));
        sram.insert("e".to_string(), Parameters::Float(E));
        while start <= end {
            x.push(start);
            if &fd == "" {
                let p = f(&vec![Parameters::Float(start)], ram);
                y.push(match p {
                    Parameters::Float(f) => f,
                    Parameters::Int(i) => i as f64,
                    Parameters::Rational(s) => s.approx(),
                    _ => f64::NAN,
                });
            } else {
                let mut names = Vec::new();
                for v in vec.clone() {
                    match v {
                        Ast::Nil => (),
                        Ast::Call { .. } => (),
                        Ast::Node {
                            value: v,
                            left: _l,
                            right: _r,
                        } => match v {
                            Parameters::Identifier(s) => names.push(s.clone()),
                            _ => (),
                        },
                    }
                }
                names
                    .iter()
                    .zip(vec![Parameters::Float(start)])
                    .for_each(|(name, param)| {
                        sram.insert(name.to_string(), param.clone());
                    });
                y.push(match interpret(&ast, &mut sram, &mut HashMap::new()) {
                    Parameters::Float(p) => p,
                    Parameters::Int(i) => i as f64,
                    Parameters::Rational(s) => s.approx(),
                    _ => f64::NAN,
                });
            }
            start += steps;
        }
    } else {
        match first_vector {
            Some(t) => {
                t.into_iter().for_each(|j| match j {
                    Parameters::Int(i) => x.push(*i as f64),
                    Parameters::Float(f) => x.push(*f),
                    Parameters::Rational(s) => x.push(s.clone().approx()),
                    Parameters::Identifier(s) => match ram.as_ref().unwrap().get(s) {
                        Some(Parameters::Int(i)) => x.push(*i as f64),
                        Some(Parameters::Float(f)) => x.push(*f),
                        Some(Parameters::Rational(r)) => x.push(r.clone().approx()),
                        _ => (),
                    },
                    _ => (),
                });
            }
            _ => return Parameters::Null,
        }

        match second_vector {
            Some(t) => {
                t.into_iter().for_each(|j| match j {
                    Parameters::Int(i) => y.push(*i as f64),
                    Parameters::Float(f) => y.push(*f),
                    Parameters::Rational(r) => y.push(r.clone().approx()),
                    Parameters::Identifier(s) => match ram.as_ref().unwrap().get(s) {
                        Some(Parameters::Int(i)) => y.push(*i as f64),
                        Some(Parameters::Float(f)) => y.push(*f),
                        Some(Parameters::Rational(r)) => y.push(r.clone().approx()),
                        _ => (),
                    },
                    _ => (),
                });
            }
            _ => return Parameters::Null,
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
        computes_lines(&x, &y, st, end, steps, title, xlabel, ylabel);
    }
    Parameters::Null
}
