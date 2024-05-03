use std::collections::HashMap;

use crate::{
    functions::add::add, functions::divide::divide, functions::function::*,
    functions::minus::minus, functions::mult::mult, parsing::ast::Parameters,
};

pub fn transpose<T>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let num_cols = matrix.first().unwrap().len();
    let mut row_iters: Vec<_> = matrix.into_iter().map(Vec::into_iter).collect();
    let mut out: Vec<Vec<_>> = (0..num_cols).map(|_| Vec::new()).collect();

    for out_row in out.iter_mut() {
        for it in row_iters.iter_mut() {
            out_row.push(it.next().unwrap());
        }
    }
    out
}

pub fn mult_matrix(
    a: Vec<Vec<Parameters>>,
    b: Vec<Vec<Parameters>>,
    ram: Option<&HashMap<String, Parameters>>,
) -> Vec<Vec<Parameters>> {
    let first = a.first().unwrap().len();
    let second = b.len();

    if first != second {
        Vec::new()
    } else {
        let n = a.len();
        let p = b.first().unwrap().len();
        let mut res = Vec::new();
        for i in 0..n {
            let mut s = Vec::new();
            for j in 0..p {
                let mut sum: Parameters = Parameters::Null;

                for k in 0..n {
                    let intermediary = mult(
                        a.get(i).unwrap().get(k).unwrap().clone(),
                        b.get(k).unwrap().get(j).unwrap().clone(),
                        ram.as_deref(),
                    );

                    sum = add(sum, intermediary, ram.as_deref())
                }

                s.push(sum);
            }
            res.push(s);
        }

        res
    }
}

pub fn lup_decompose(
    a: &mut Vec<Vec<Parameters>>,
    mut p: &mut Vec<Parameters>,
    n: usize,
    ram: Option<&HashMap<String, Parameters>>,
) -> i64 {
    let mut abs_a;
    let mut max_a;
    let mut ptr: Vec<Parameters>;
    let mut i_max: usize;

    for i in 0..(n + 1) {
        (&mut p)[i] = Parameters::Int(i as i64);
    }

    for i in 0..n {
        max_a = Parameters::Float(0.0);
        i_max = i;

        for k in i..n {
            abs_a = ((a[k])[i]).clone().abs(ram.as_deref());
            match greater(abs_a.clone(), max_a.clone(), ram.as_deref()) {
                Parameters::Bool(true) => {
                    max_a = (abs_a).clone();
                    i_max = k;
                }
                _ => (),
            }
        }

        match max_a {
            Parameters::Int(0) => return 0,
            Parameters::Float(f) => {
                if f.abs() <= 1e-10 {
                    return 0;
                }
            }
            _ => (),
        }

        if i_max != i {
            let j = p[i].clone();
            p[i] = p[i_max].clone();
            (p)[i_max] = j.clone();

            ptr = (a)[i].clone();
            (a)[i] = (a)[i_max].clone();
            (a)[i_max] = ptr.clone();

            (p)[n] = add((p)[n].clone(), Parameters::Int(1), ram.as_deref());
        }

        for j in (i + 1)..n {
            (a)[j][i] = divide((a)[j][i].clone(), (a)[i][i].clone(), ram.as_deref());
            for k in (i + 1)..n {
                (a)[j][k] = minus(
                    (a)[j][k].clone(),
                    mult((a)[j][i].clone(), (a)[i][k].clone(), ram.as_deref()),
                    ram.as_deref(),
                )
            }
        }
    }
    return 1;
}

pub fn lup_determinant(
    a: &mut Vec<Vec<Parameters>>,
    p: &mut Vec<Parameters>,
    n: usize,
    ram: Option<&HashMap<String, Parameters>>,
) -> Parameters {
    let mut det: Parameters = (&a[0][0]).clone();
    for i in 1..n {
        det = mult(det.clone(), (&a[i][i]).clone(), ram.as_deref())
    }

    match p[n] {
        Parameters::Int(i) => {
            if (i - (n as i64)) % 2 == 0 {
                det
            } else {
                minus(Parameters::Int(0), det, ram.as_deref())
            }
        }
        Parameters::Float(f) => {
            if (f - (n as f64)) % 2.0 == 0.0 {
                det
            } else {
                minus(Parameters::Float(0.0), det, ram.as_deref())
            }
        }
        _ => Parameters::Float(f64::NAN),
    }
}

pub fn lup_invert(
    a: &mut Vec<Vec<Parameters>>,
    p: &mut Vec<Parameters>,
    n: usize,
    ia: &mut Vec<Vec<Parameters>>,
    ram: Option<&HashMap<String, Parameters>>,
) {
    for j in 0..n {
        for i in 0..n {
            ia[i][j] = match &p[i] {
                Parameters::Int(s) => {
                    if *s == j as i64 {
                        Parameters::Int(1)
                    } else {
                        Parameters::Int(0)
                    }
                }
                Parameters::Float(f) => {
                    if (*f - (j as f64)).abs() <= 1e10 {
                        Parameters::Int(1)
                    } else {
                        Parameters::Int(0)
                    }
                }
                Parameters::Identifier(s) => match ram {
                    None => Parameters::Int(0),
                    Some(hs) => match hs.get(s.as_str()) {
                        None => Parameters::Int(0),
                        Some(Parameters::Int(s)) => {
                            if (*s - (j as i64)) == 0 {
                                Parameters::Int(1)
                            } else {
                                Parameters::Int(0)
                            }
                        }
                        Some(Parameters::Float(f)) => {
                            if (*f - (j as f64)).abs() <= 1e-10 {
                                Parameters::Int(1)
                            } else {
                                Parameters::Int(0)
                            }
                        }
                        _ => Parameters::Int(0),
                    },
                },
                _ => Parameters::Int(0),
            };
            for k in 0..i {
                ia[i][j] = minus(
                    (&ia[i][j]).clone(),
                    mult((&a[i][k]).clone(), (&ia[k][j]).clone(), ram.as_deref()),
                    ram.as_deref(),
                );
            }
        }

        for i in (0..n).rev() {
            for k in i + 1..n {
                ia[i][j] = minus(
                    (&ia[i][j]).clone(),
                    mult((&a[i][k]).clone(), (&ia[k][j]).clone(), ram.as_deref()),
                    ram.as_deref(),
                )
            }
            ia[i][j] = divide((&ia[i][j]).clone(), (&a[i][i]).clone(), ram.as_deref());
        }
    }
}

#[cfg(test)]
mod test {

    use crate::{
        functions::function::greater, functions::minus::minus, parsing::ast::Parameters,
        utils::matrix_utils::lup_determinant,
    };

    use super::lup_decompose;

    #[test]
    pub fn test() {
        let mut a = vec![
            vec![
                Parameters::Float(1.0),
                Parameters::Float(2.0),
                Parameters::Float(3.0),
            ],
            vec![
                Parameters::Float(4.0),
                Parameters::Float(0.0),
                Parameters::Float(6.0),
            ],
            vec![
                Parameters::Float(7.0),
                Parameters::Float(8.0),
                Parameters::Float(9.0),
            ],
        ];

        let mut b = vec![Parameters::Int(0); 4];

        let _ = lup_decompose(&mut a, &mut b, 3 as usize, None);

        println!("{:?}/{:?}", &a, &b);

        let det = lup_determinant(&mut a, &mut b, 3 as usize, None);

        println!("{:?}", det);
        assert_eq!(
            greater(
                Parameters::Float(1e-10),
                minus(det, Parameters::Float(60.0), None).abs(None),
                None
            ),
            Parameters::Bool(true)
        );
    }
}
