#import "@preview/codly:1.0.0": *
#import "@preview/colorful-boxes:1.4.1": *

#set page(numbering: "1/1")
#set align(center)
#set text(font: "Monaspace Xenon")
#set text(size: 18pt, weight: "bold")
Calc Manual\
#set text(size: 13pt, weight: "regular")
Last updated, #datetime.today().display()
#set align(left)
#show heading.where(level: 1): set align(right)
#set heading(numbering: "I.1.")

#set list(indent: 1cm, body-indent: 0.25cm)

#outline(title: [Table of Contents], indent: auto, depth: 2)
#pagebreak(weak: true)

#let icon(codepoint) = {
  box(
    height: 0.8em,
    baseline: 0.05em,
    image(codepoint),
  )
  h(0.1em)
}

#show: codly-init.with()
#codly(languages: (
  rust: (name: "Rust", icon: icon("brand-rust.svg"), color: rgb("#CE412B")),
  sh: (name: "Bash", icon: icon("brand-bash.svg"), color: rgb("3c3c3c")),
))

#let calc = link("https://www.charlotte-thomas.me", [#set text(red); *Calc*])

= Introduction
#v(1em)
#calc is a fully-featured calculator written in Rust for education purpose, it
was designed to be minimalistic but then went off the rails and a lot of feature
where implemented.

Now #calc is a powerful calculator capable of exact rational computation,
matrix and vectors algebra, symbolic computation, differentiation, with bindings
to gnuplot and terminal plotting, alongside
dozens of updates and currently (as of writing this manual) in version *3.3.3*.

If you prefer a website you may want to read
#link("https://calc.charlotte-thomas.me/book",[*The Online Book (SOON)*]) which is always up to
date.

== Install

You can install it via cargo

```sh
cargo install mini-calc
```

or via the source

```sh
git clone https://github.com/vanilla-extracts/calc
cd calc
cargo build --release
./target/release/mini-calc
```

or alternatively, build/run using nix

```sh
nix run github.com:vanilla-extracts/calc
```

Visit #calc to see all the install page

== Contributors


#table(
  columns: (auto, auto, auto),
  inset: 10pt,
  align: horizon,
  [*Name*], [*Role*], [*Website*],
  "Charlotte THOMAS",
  "Main developer/Maintener",
  link("https://www.charlotte-thomas.me", [#set text(red); Personal Page]),

  "Léana 江", "Help, cleanup", link("https://earth2077.fr", [#set text(red); Website/Blog]),
)

#pagebreak(weak: true)

= Usage

== Basic operators
#calc have the basic operators which are

- `+` for the addition
- `-` for the substraction
- `*` for the multiplication
- `/` for the division (or for a rational)
- `^` for the exponentation

#colorbox(
 title: "Warning",
 color: "red",
)[
  You shoudln't use the `^` operator with functions, it is not taken into
  account for differentiations
]

== Variables

It also supports variable the syntax is

```
myvar = value
```

for example

```
var = (2+2)
```

#set align(center)
#figure(
  image("assets/image.png", height: 30%, width: auto),
  caption: [Example of setting a variable],
)
#set align(left)

== Built-in variables

The following variables are built-in:

- `pi` is pi as a double precision float
- `e` is e as a double precision float

#pagebreak(weak: true)

= Functions

== Implemented

The following functions are currently implemented:

*Trigonometry*

- `sin` (vectorized)
- `cos` (vectorized)
- `tan` (vectorized)

*Hyperbolic trigonometry*
- `sinh` (vectorized)
- `cosh` (vectorized)
- `tanh` (vectorized)

*Reverse trigonometry*
- `acos` (vectorized)
- `asin` (vectorized)
- `atan` (vectorized)

*Exponentiation*
- `exp` (vectorized)
- `ln` (alias: log) (vectorized)

*Vectors*
- `norm`

*Matrices*
- `det`
- `invert`

*Plot*
- `plot`
- `termplot`

*Other*
- `sqrt` (vectorized)
- `factorial` (alias: fact)
- `abs`
- `ceil`
- `floor`
- `round`

== Trigonometry

For trigonometry, the input is assumed to be in radians, if it is in degrees you
need to add `false` or `true` as a second argument, example shown bellow.

#set align(center)
#figure(
  image("assets/trigo.png"),
  caption: [Usage of trigonometry],
)
#set align(left)

#pagebreak(weak: true)
== Exp/ln

If you use the exp function you can pass as a second argument the base you want
to use if no second arguments are passed it will used the natural base.

#set align(center)
#figure(
  image("assets/expln.png"),
  caption: [Usage of exp/ln],
)
#set align(left)

#pagebreak(weak: true)
== Root

You can specify in second argument an integer to take the nth root, if not it
take the square root.

#set align(center)
#figure(
  image("assets/nth_root.png"),
  caption: [Usage of sqrt],
)
#set align(left)

== Partial function

The calculator's language supports partial function.

#set align(center)
#figure(
  image("assets/function.png"),
  caption: [Example of a partial function],
)
#set align(left)

#pagebreak(weak: true)
== Vectorization
Functions have been vectorized.

#set align(center)
#figure(
  image("assets/sqrt_vectorized.png"),
  caption: [Example of a vectorized function],
)
#set align(left)

== User defined function
You can defined your own function

#figure(
  image("assets/user_defined.png"),
  caption: [Definition of function],
)
#pagebreak(weak: true)

= Configuration

You can configure the general color, greeting message, greeting color, prompt
and prompt color in a toml file found for example on linux in

```sh
~/.config/mini-calc/mini-calc.toml
```

#set align(center)
#figure(
  image("assets/img.png"),
  caption: [Example of the default configuration],
)
#set align(left)

== Colors
Available colors are

- blue
- black
- purple
- green
- cyan
- red
- yellow
- white
- an hexadecimal color (ex: \#f7d8a8)

The default color (or if your colour can't be parsed) is cyan

#pagebreak(weak: true)

== Example of a modified configuration

#set align(center)
#figure(
  image("assets/config_modified.png"),
  caption: [Example of a modified config],
)
#set align(left)

it looks like

#set align(center)
#figure(
  image("assets/config_looks.png"),
  caption: [Modified configuration in action],
)
#set align(left)

== Interact in the command line

You can interact in the command line with the config, the commands are

- config: show the config help
- config reload: reload the config from the file
- config reset: reset the config
- config show: show the current config
- config set `<category>` `<value>`

categories are:

- greeting_message
- greeting_color
- prompt_color
- prompt
- general_color

#figure(
  image("assets/config.png"),
  caption: [Example of interaction in the command line of config],
)

#pagebreak(weak: true)
= Logic

== Implemented operators

The following operators have been implemented:

- or (alias: `||`)
- and (alias: `&&`)
- geq (alias: `>=`)
- leq (alias: `<=`)
- gt (alias : `>`)
- lt (alias: `<`)

== Example

#figure(
  image("assets/logic.png"),
  caption: [Example of logic],
)

#pagebreak(weak: true)

= Plot
You can plot, the backend is provided by GNUPlot, so it should work great on
linux and macos, the behaviour on windows is unknown.

== Help
To display the help just type `plot()`

#figure(
  image("assets/plot_help.png"),
  caption: [Help of plot],
)

== Plot

=== Default
It's easy to plot a function just type `plot(fn)`

#figure(
  image("assets/plot_cos_default.png"),
  caption: [Plot of the cos function with default values],
)

#pagebreak(weak: true)
=== Options

A more difficult operation is with values, `plot(sin,-pi,pi,0.01,"sin","x(rad)","y","line")`.

#figure(
  image("assets/plot_sin_custom.png"),
  caption: [Plot with overloading of default values],
)

=== Plot your own function

You can plot your own defined functions!

#figure(
  image("assets/plot_f.png"),
  caption: [Plot of an user-defined function],
)

#pagebreak(weak: true)
== Terminal plotting
You can plot _right_ into your terminal

=== Default

The best example to show it is the square function between -5 and 5 with a 0.1 step. The x axis is automatically scaled but not the y axis for now.

#figure(
  image("assets/plot_term_x_squared.png"),
  caption: [Terminal plotting of an user defined function],
)

#pagebreak(weak: true)
=== Options
The terminal supports labels as options

#figure(
  image("assets/plot_term_x_squared_labels.png"),
  caption: [Terminal plotting with options],
)
#pagebreak(weak: true)
=== Auto y scaling
It scales automatically in y too!

#figure(
  image("assets/termplot_cos.png"),
  caption: [Example of a plot with y auto-scaling],
)
#pagebreak(weak: true)

= Vectors computation

You can compute vectors using these functions,

- add vectors
- dot product (\* operator)
- norm function

#figure(
  image("assets/vector.png"),
  caption: [Example of vector computation],
)

= Matrices computation

As of 2.7.0 matrix algebra was added to the calculator you can

- add matrices
- multiply compatible matrices

functions added

- transpose
- invert
- 2023-11-26 14:49

#figure(
  image("assets/matrix.png"),
  caption: [Example of matrix computation],
)

And as of 2.11.5 they are pretty printed with each column being aligned.

#figure(
  image("assets/aligned_matrices.png"),
  caption: [Pretty printed matrix],
)

#pagebreak(weak: true)
= Exact math

== Rational exact math

As of 2.11.0 rational exact math was added, supports for

- rational operations
- rational reduction
- rationalization of floats (precision: 10 digits)

=== Examples

#figure(
  image("assets/exact_rationals.png"),
  caption: [Example of rational computations],
)

#figure(
  image("assets/exact_inverse.png"),
  caption: [Example of rational in matrices],
)
#pagebreak()

== Symbolic computation

As of 3.0.0, the support for symbolic reduction has been added to calc.
It supports multi-variable multi-operation reductions, you can reduce
expressions using it, fair warning, it uses _plenty_ of parentheses!

#figure(
 image("assets/multi_variable.png"),
 caption: [Example of a multi-variable reduction]
)

== Function differentiation 

As of `3.2.0`, the calculator can differentiate known functions (function
constructed using the standard functions). It supports both built-in and
user-defined functions. 

#colorbox(title: "Warning", color: "red")[Beware as of `3.3.3` there is some bugs to iron out.
It doesn't differentiates vars (differentiation of $x*x$ works but not $x^2$.).
And differentiation of function 
referencing each other (example $v(x) = f(x) + x*x$) doesn't work either.] 

#colorbox(title: "Fixed", color: "blue")[As of `3.3.4` functions referencing
each other works with *diff*]

=== Examples

#figure( 
 image("assets/diff_builtin.png"), 
caption: [Example of a built-in function differentiation]
)

#figure( 
 image("assets/diff_ud.png"), 
caption: [Example of a user-defined function differentiation] 
)

#pagebreak(weak: true)
= Non interactive use

With version 2.12.0 non interactive use was added to the calculator if you have
a quick computation to run and don't want to start the REPL.

#figure(
  image("assets/non_interactive_use.png"),
  caption: [Example of non interactive use],
)

#pagebreak(weak: true)
= Syntax coloration

In 3.3.0 the coloration was added.

#figure( 
image("assets/syntax_coloration.png"), 
caption: [Syntax coloration as of `3.3.3`]
)

= Float display
With `v3.4.0` there is now a way to chose how you want your floats to be
displayed.

== Modes

For example in `normal` mode if you type 
```
> 1.5
```

It outputs: 

```
val: float = 1.5000000000
```

In `exact` mode, that
```
> 1.5
```

Outputs:

```
val: rational = 3/2
```

And finally in `science` mode, this
```
> 150.2
```

Outputs

```
val: float = 1.502*10²
```

== Toggle float
There is a `toggle_float` command in the REPL now.

Its usage is easy and it is auto-completed.
```
> toggle_float <normal|science|exact>
```

It then displays a message telling you in which mode you toggled.

By default it is in `exact` mode.

== Example
#figure(
image("assets/float_mode.png"),
caption: "The different float modes"
)

== Default float mode
As of `v3.4.2` you can alter the default float mode.
By default it is `exact`

You can alter it by modifying the config file directly or by using the `config set` command.

```
> config set float_mode exact|normal|science
```

And then reload the config with
```
> config reload
```

#colorbox(color: "red", title: "Warning")[
  The addition of `default_float_mode` in config *resets* your config.
  
  If you don't want your config to be overwritten, add the following in your
  config (e.g `$HOME/.config/mini-calc/mini-calc.toml`)

  ```toml
  default_float_mode = 'exact|science|normal'
  ```
]
