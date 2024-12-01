# Functions

## Built-in functions
The following functions are currently (as of `3.3.0`) built-in

- Trigonometry 
  - `sin` (vectorised)
  - `cos` (vectorised)
  - `tan` (vectorised)
- Hyperbolic trigonometry
  - `sinh` (vectorised)
  - `cosh` (vectorised)
  - `tanh` (vectorised)
- Reverse trigonometry
  - `asin` (vectorised)
  - `acos` (vectorised)
  - `atan` (vectorised)
- Exponentiation
  - `exp` (vectorised)
  - `ln` (alias `log`) (vectorised)
- Vectors
  - `norm` 
- Matrices
  - `det`
  - `invert`
- Plot 
  - `plot`
  - `termplot`
- Other
  - `sqrt` (vectorised)
  - `factorial` (alias `fact`)
  - `abs`
  - `ceil` 
  - `floor`
  - `round`

## Trigonometry

For trigonometry, the input is assumed to to in `radians`, if it is in `degrees`
you need to add a second argument (can be anything, like `true`) to the function
call.

![example](/assets/trigo.png)

## Exp/ln 
If you use the `exp` function you can pass the base you want to work with 
as a second argument. If nothing is supplied, we assume natural base.

![example](/assets/expln.png)

## Root
You can add a second argument to `sqrt` for the `nth` root instead 
of the 2nd root.

![example](/assets/nth_root.png)

## User defined functions
You can define your own functions with the following syntax
(warning: it might change in `4.0.0`)

![example](/assets/user_defined.png)

## Partial functions
You can use a user-defined function to declare a partial function, 
to have a cos in degrees for example.

![example](/assets/function.png)
