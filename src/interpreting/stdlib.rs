use std::collections::HashMap;
use std::f64::consts::{E, PI};

use gnuplot::{AxesCommon, Figure};

use crate::configuration::loader::{load, load_config, Config};
use crate::functions;
use crate::interpreting::interpreter::interpret;
use crate::parsing::ast::{Ast, Parameters, Parameters::*};
use crate::utils::matrix_utils::{lup_decompose, lup_determinant, lup_invert, transpose};
use crate::utils::plot_utils::computes_lines;

use crate::functions::add::add as other_add;
use crate::functions::mult::mult;

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
        "diff" => diff(&lst, &ram, functions),
        s => {
            let mut sram: HashMap<String, Parameters> = HashMap::new();
            sram.insert("pi".to_string(), Float(PI));
            sram.insert("e".to_string(), Float(E));
            match functions.cloned() {
                None => Identifier("This function is unknown".to_string()),
                Some(mut f) => {
                    let fs = f.get_mut(s);
                    let (vec, ast): (&mut Vec<Ast>, &mut Ast) = match fs {
                        None => {
                            return Identifier("This function is unknown".to_string());
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
                                Identifier(s) => names.push(s.clone()),
                                _ => (),
                            },
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
                    interpret(ast, &mut sram, &mut HashMap::new())
                }
            }
        }
    }
}

pub fn cos(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Float(fs.cos())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.cos())
        }
        Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
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
                                res.push(cos(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(cos(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        cos(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        cos(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn sin(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Float(fs.sin())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.sin())
        }
        Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
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
                                res.push(sin(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(sin(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        sin(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        sin(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn tan(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Float(fs.tan())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.tan())
        }
        Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
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
                                res.push(tan(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(tan(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        tan(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        tan(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn cosh(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Float(fs.cosh())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.cosh())
        }
        Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
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
                                res.push(cosh(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(cosh(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        cosh(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        cosh(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn sinh(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Float(fs.sinh())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.sinh())
        }
        Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
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
                                res.push(sinh(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(sinh(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        sinh(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        sinh(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn tanh(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
        Int(i) => {
            let fs: f64 = if degrees {
                ((*i).clone() as f64) * (PI / 180.0)
            } else {
                (*i).clone() as f64
            };
            Float(fs.tanh())
        }
        Float(f) => {
            let fs: f64 = if degrees { (*f) * (PI / 180.0) } else { *f };
            Float(fs.tanh())
        }
        Rational(s) => {
            let fs = if degrees {
                s.clone().approx() * PI / 180.0
            } else {
                s.clone().approx()
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
                                res.push(tanh(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(tanh(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        tanh(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        tanh(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn acos(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
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
            s.clone().approx().acos() * 180.0 / PI
        } else {
            s.clone().approx().acos()
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
                                res.push(acos(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(acos(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        acos(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        acos(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn asin(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
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
            s.clone().approx().asin() * (180.0 / PI)
        } else {
            s.clone().approx().asin()
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
                                res.push(asin(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(asin(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        asin(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        asin(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn atan(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    let mut degrees = false;

    if p.len() > 1 {
        match p.get(1) {
            None => degrees = false,
            Some(_) => degrees = true,
        }
    }

    match p.get(0).unwrap() {
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
            s.clone().approx().atan() * (180.0 / PI)
        } else {
            s.clone().approx().atan()
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
                                res.push(atan(&vec![s.clone(), Bool(false)], ram))
                            } else {
                                res.push(atan(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => {
                    if degrees {
                        atan(&vec![t.clone(), Identifier("false".to_string())], ram)
                    } else {
                        atan(&vec![t.clone()], ram)
                    }
                }
            },
        },
        _ => Null,
    }
}

pub fn exp(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
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

    match p.get(0).unwrap() {
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
                Float(ln.powf(s.clone().approx()))
            } else {
                Float(s.clone().approx().exp())
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
                                res.push(exp(&vec![s.clone(), Float(ln)], ram))
                            } else {
                                res.push(exp(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => exp(&vec![t.clone(), Float(ln)], ram),
            },
        },
        _ => Null,
    }
}

pub fn ln(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
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

    match p.get(0).unwrap() {
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
                Float(s.clone().approx().log(sln))
            } else {
                Float(s.clone().approx().ln())
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
                                res.push(ln(&vec![s.clone(), Float(sln)], ram))
                            } else {
                                res.push(ln(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => ln(&vec![t.clone(), Float(sln)], ram),
            },
        },
        _ => Null,
    }
}

pub fn sqrt(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
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

    match p.get(0).unwrap() {
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
                Float(s.clone().approx().powf(1.0 / sln))
            } else {
                Float(s.clone().approx().sqrt())
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
                    s.clone().approx().powf(1.0 / sln)
                } else {
                    s.clone().approx().sqrt()
                })),
                Identifier(s) => match ram {
                    None => (),
                    Some(ref t) => match t.get(s.as_str()) {
                        None => (),
                        Some(s) => {
                            if plus {
                                res.push(sqrt(&vec![s.clone(), Float(sln)], ram))
                            } else {
                                res.push(sqrt(&vec![s.clone()], ram))
                            }
                        }
                    },
                },
                _ => (),
            });
            InterpreterVector(Box::from(res))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => sqrt(&vec![t.clone(), Float(sln)], ram),
            },
        },
        _ => Null,
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
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Int(fact(*i)),
        Float(f) => Parameters::Int(fact(*f as i64)),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => factorial(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn abs(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Int(i.abs()),
        Float(f) => Parameters::Float(f.abs()),
        Rational(s) => Parameters::Rational(s.clone().abs()),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => abs(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn ceil(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Float((*i as f64).ceil()),
        Float(f) => Parameters::Float(f.ceil()),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => ceil(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn floor(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Float((*i as f64).floor()),
        Float(f) => Parameters::Float(f.floor()),
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => floor(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn round(p: &Vec<Parameters>, ram: &Option<&mut HashMap<String, Parameters>>) -> Parameters {
    if p.len() < 1 {
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

    match p.get(0).unwrap() {
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
                Float((s.clone().approx() * 10.0_f64.powf(sln).round()) / (10.0_f64.powf(sln)))
            } else {
                Float(s.clone().approx().round())
            }
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => round(&vec![t.clone(), Float(sln)], ram),
            },
        },
        _ => Null,
    }
}

pub fn norm(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
    function: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
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
                Some(t) => norm(&vec![t.clone()], ram, function),
            },
        },
        _ => Null,
    }
}

pub fn transpose_vectors(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational(s.clone().abs()),
        InterpreterVector(lst) => {
            let r = vec![*(lst.clone())];
            let transposed = transpose(r);

            let mut result = Vec::new();

            transposed
                .into_iter()
                .map(|v| InterpreterVector(Box::from(v)))
                .for_each(|v| result.push(v));

            InterpreterVector(Box::from(result))
        }
        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => transpose_vectors(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn transpose_matrices(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational(s.clone().abs()),
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
                .for_each(|x| result.push(InterpreterVector(Box::from(x))));
            InterpreterVector(Box::from(result))
        }

        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => transpose_matrices(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn det_matrix(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational(s.clone().abs()),
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
                Some(t) => det_matrix(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn inverse_matrix(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
) -> Parameters {
    if p.len() < 1 {
        return Null;
    }

    match p.get(0).unwrap() {
        Int(i) => Parameters::Int((*i).abs()),
        Float(f) => Parameters::Float((*f).abs()),
        Rational(s) => Parameters::Rational(s.clone().abs()),
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
                return InterpreterVector(Box::from(res1));
            }

            let mut p = Vec::new();
            for _ in 0..(res.len() + 1) {
                p.push(Int(0));
            }
            let n = res.len();
            let r = lup_decompose(&mut res, &mut p, n, ram.as_deref());

            match r {
                0 => Null,
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
                            return Str("Determinant is zero, matrix is not invertible".to_string())
                        }
                        Float(s) if s.abs() < 1e-10 => {
                            return Str("Determinant is zero, matrix is not invertible".to_string())
                        }
                        Rational(s) if s.clone().is_null() => {
                            return Str("Determinant is zero, matrix is not invertible".to_string())
                        }
                        _ => (),
                    }
                    lup_invert(&mut res, &mut p, n, &mut vec_ia, ram.as_deref());
                    let mut resd = Vec::new();
                    for i in 0..n {
                        resd.push(InterpreterVector(Box::new(vec_ia[i].clone())));
                    }
                    InterpreterVector(Box::new(resd))
                }
            }
        }

        Identifier(s) => match ram {
            None => Identifier("This variable is not initialized yet".to_string()),
            Some(ref t) => match t.get(s.as_str()) {
                None => Null,
                Some(t) => inverse_matrix(&vec![t.clone()], ram),
            },
        },
        _ => Null,
    }
}

pub fn diff(
    p: &Vec<Parameters>,
    ram: &Option<&mut HashMap<String, Parameters>>,
    function: Option<&mut HashMap<String, (Vec<Ast>, Ast)>>,
) -> Parameters {
    let color = match load() {
        Ok(cfg) => load_config(cfg).general_color,
        Err(_) => load_config(Config::default()).general_color,
    };

    if p.len() == 0 {
        let m = color.paint("Usage: diff <function>");
        println!("{m}");
        return Null;
    }

    let first_param = p.first().unwrap();

    match first_param {
        Identifier(fun) => match fun.as_str() {
            "cos" => Identifier("-sin(x)".to_string()),
            "sin" => Identifier("cos(x)".to_string()),
            "exp" => Identifier("exp(x)".to_string()),
            "ln" => Identifier("1/x".to_string()),
            "tan" => Identifier("1/(cos(x)*cos(x))".to_string()),
            "sinh" => Identifier("sinh".to_string()),
            "cosh" => Identifier("cosh".to_string()),
            "acos" => Identifier("(-1)/(sqrt(1 - x*x))".to_string()),
            "asin" => Identifier("1/(sqrt(1-x*x))".to_string()),
            "x" => Identifier("1".to_string()),
            "sqrt" => Identifier("1/(2*sqrt(x))".to_string()),
            p => {
                let mut c: HashMap<String, Parameters> = HashMap::new();
                for (key, ele) in ram.as_deref().unwrap().clone() {
                    c.insert(key, ele);
                }
                let param = exec(
                    p.to_string(),
                    vec![Identifier("x".to_string())],
                    Some(&mut c),
                    function,
                );
                println!("{:#?}", param);
                match param {
                    Identifier(_) => Int(1),
                    Var(x, y, z) => Var(
                        Box::from(mult(Parameters::Int(y), *x.clone(), Some(&c))),
                        y - 1,
                        z,
                    ),
                    _ => Int(0),
                }
            } //2*x = 2'*x + 2*x' = 0*x + 2
        },
        _ => Null,
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
        return Null;
    }

    let fs = p.first().unwrap();
    let mut f: fn(&Vec<Parameters>, &Option<&mut HashMap<String, Parameters>>) -> Parameters = cos;
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
            Rational(s) => start = s.clone().approx(),
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
            Rational(s) => end = s.clone().approx(),

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
            Float(f) => steps = *f,
            Int(i) => steps = *i as f64,
            Rational(s) => steps = s.clone().approx(),

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
            Str(s) => match s.to_lowercase().as_str() {
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
            Str(s) => match s.to_lowercase().as_str() {
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
            Str(s) => match s.to_lowercase().as_str() {
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
            Str(s) => match s.to_lowercase().as_str() {
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
        sram.insert("pi".to_string(), Float(PI));
        sram.insert("e".to_string(), Float(E));
        while start <= end {
            x.push(start);
            if &fd == "" {
                let p = f(&vec![Float(start)], ram);
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
                        Ast::Node {
                            value: v,
                            left: _l,
                            right: _r,
                        } => match v {
                            Identifier(s) => names.push(s.clone()),
                            _ => (),
                        },
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
                t.into_iter().for_each(|j| match j {
                    Int(i) => x.push(*i as f64),
                    Float(f) => x.push(*f),
                    Rational(s) => x.push(s.clone().approx()),
                    Identifier(s) => match ram.as_ref().unwrap().get(s) {
                        Some(Int(i)) => x.push(*i as f64),
                        Some(Float(f)) => x.push(*f),
                        Some(Rational(r)) => x.push(r.clone().approx()),
                        _ => (),
                    },
                    _ => (),
                });
            }
            _ => return Null,
        }

        match second_vector {
            Some(t) => {
                t.into_iter().for_each(|j| match j {
                    Int(i) => y.push(*i as f64),
                    Float(f) => y.push(*f),
                    Rational(r) => y.push(r.clone().approx()),
                    Identifier(s) => match ram.as_ref().unwrap().get(s) {
                        Some(Int(i)) => y.push(*i as f64),
                        Some(Float(f)) => y.push(*f),
                        Some(Rational(r)) => y.push(r.clone().approx()),
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
        computes_lines(&x, &y, st, end, steps, title, xlabel, ylabel);
    }
    Null
}
