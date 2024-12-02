# Section VII.3. Function differentiation
As of `v3.2.0`, the calculator can differentiate both known functions (built-in)
and user-defined functions constructed using known functions and operations.

<h2 style="color:red;">Warning</h2> 

Beware, as of `v3.3.3` there is some bugs to iron out. It doesn't 
differentiate variables (for example `x*x` works but not `x²`). 
And differentiation of function referencing each other 
(for example `v(x) = f(x) + x*x`) doesn't work either.

It's currently being fixed, and will be fixed before stable release `v3.4.0` 
which is currently in `alpha` development-testing (it will implements basic languages
features 
such as flow control and condition.)

The release `v3.3.4`, which correct those bugs is currently in `beta` dev-testing 
and will be released before `v3.4.0`.

## Examples
### Built-in 
![diff_bi](/assets/diff_builtin.png)

### User defined
![ud](/assets/diff_ud.png)
