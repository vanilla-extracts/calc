pub fn computes_lines(
    x: &Vec<f64>,
    y: &Vec<f64>,
    start: f64,
    end: f64,
    _steps: f64,
    title: String,
    xlabel: String,
    ylabel: String,
) -> () {
    let mut bitmap = vec![vec![' '; 100]; 30];

    let mut ymin = f64::MAX;
    let mut ymax = f64::MIN;

    y.into_iter().for_each(|y| {
        if y > &ymax {
            ymax = *y
        }
        if y < &ymin {
            ymin = *y
        }
    });

    let mut x_scale = (end - start) / 99.0;
    let mut y_scale = (ymax - ymin) / 29.0;

    if x_scale == 0.0 {
        x_scale = 1.0;
    }
    if y_scale == 0.0 {
        y_scale = 1.0;
    }

    let z = x.into_iter().zip(y).map(|(x, y)| {
        (
            ((*x - start) / x_scale) as usize,
            ((*y - ymin) / y_scale) as usize,
        )
    });

    z.for_each(|(x, y)| {
        if x < 100 && y < 30 {
            bitmap[y][x] = '+';
        }
    });

    let first_line = vec!['*'; 104];
    let last_line = vec!['*'; 104];

    let x_line = vec!['-'; 100];
    for char in first_line {
        print!("{char}");
    }

    println!("");

    if &title != "" {
        let left_padding = (104 - title.len()) / 2;
        let right_padding = (104 - title.len()) - left_padding;
        for _ in 0..left_padding {
            print!("*");
        }
        print!("{}", title);
        for _ in 0..right_padding {
            print!("*")
        }

        println!("");
    }

    let size = ylabel.len();
    let i = ylabel.chars();
    let mut label = Vec::new();
    let f = 30 / 2 - size;
    let e = 30 - f - size;
    for _ in 0..f {
        label.push('*')
    }
    i.for_each(|x| label.push(x));
    for _ in 0..e {
        label.push('*')
    }
    let mut iter_label = label.into_iter();

    let lsize = format!("{:.2}", ymax).len();
    let string_ymax = format!("{:.2}", ymax);
    let mut y_sized = Vec::new();
    let lminsize = format!("{:.2}", ymin).len();
    let lmin_string = format!("{:.2}", ymin);
    let ymiddle = format!("{:.2}", (ymax + ymin) / 2.0);
    let ymiddle_size = ymiddle.len();

    for s in string_ymax.replace("-", "|").chars().rev() {
        y_sized.push(s);
    }
    for _ in (lsize)..(30 / 2) {
        y_sized.push(' ');
    }
    for s in ymiddle.replace("-", "|").chars().rev() {
        y_sized.push(s);
    }
    for _ in ymiddle_size..(30 / 2 - lminsize) {
        y_sized.push(' ');
    }
    for s in lmin_string.replace("-", "|").chars().rev() {
        y_sized.push(s);
    }

    let mut iter_y_sized = y_sized.into_iter();

    for x in (0..(bitmap.len())).rev() {
        print!("{}", iter_label.next().unwrap());
        print!("{}", iter_y_sized.next().unwrap());
        print!("|");
        let xs = &bitmap[x];
        for y in 0..xs.len() {
            print!("{}", xs[y]);
        }
        print!("*\n");
    }

    print!("* |");
    for char in x_line {
        print!("{char}");
    }
    println!("*");

    print!("* ");
    let string_start = format!("{:.2}", start).len();
    let string_end = format!("{:.2}", end).len();
    let middle = ((end + start) / 2.0) as f32;
    let middle_string = format!("{:.2}", middle).len();
    print!("{:.2}", start);
    for _ in (string_start)..(100 / 2) {
        print!(" ");
    }
    print!("{:.2}", middle);
    for _ in (middle_string)..(100 / 2 - 1 - string_end) {
        print!(" ");
    }
    println!("{:.2}  *", end);

    if &xlabel != "" {
        let first = 104 / 2 - xlabel.len();
        let last = 104 - first - xlabel.len();
        for _ in 0..first {
            print!("*")
        }
        print!("{xlabel}");
        for _ in 0..last {
            print!("*")
        }
    } else {
        for char in last_line {
            print!("{char}");
        }
    }

    println!("");
}
