# Section VII.4. Float display
With `v3.4.0` there is now a way to chose how you want your floats to be
displayed.

## Modes

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

## Toggle float
There is a `toggle_float` command in the REPL now.

Its usage is easy and it is auto-completed.
```
toggle_float <normal|science|exact>
```

It then displays a message telling you in which mode you toggled.

By default it is in `exact` mode.

## Example
![float_mode](/assets/float_mode.png)
