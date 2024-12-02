# Chapter III. Configuration
The calculator is completely configurable, you can change the general color,
the greeting message, the greeting color, the prompt and prompt color
in a toml file found in your config folder.

```sh 
$HOME/.config/mini-calc/mini-calc.toml
```

On most GNU+Linux distros.

![config](/assets/img.png)

## Colors
You can use the following colors:

- blue
- black
- purple
- green
- cyan
- red
- yellow
- white
- an hexadecimal color (ex: #f7a8d8)

The default color (or if it can't be parsed) is `cyan`.

## Examples

A modified config might looks like this 

![mod_config](/assets/config_modified.png)

And produce the following output 

![mod_config_look](/assets/config_looks.png)

## Command line interaction

You can interact in the REPL with the config the commands are:

- `config`: shows the config help.
- `config reload`: reloads the configuration from the file.
- `config reset`: resets the configuration to its default.
- `config show`: shows the current configuration.
- `config set <category> <value>`: modifies the configuration. 

Categories are 

- `greeting_message`: string 
- `greeting_color`: color 
- `prompt_color`: color
- `prompt`: string
- `general_color`: color

![conf_inter](/assets/config.png)
