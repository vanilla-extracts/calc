use ansi_term::{ANSIGenericString, Color};
use confy::ConfyError;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct Greeting {
    pub greeting_message: String,
    pub greeting_color: String,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Prompt {
    pub prompt: String,
    pub prompt_color: String,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Config {
    pub general_color: String,
    pub greeting: Greeting,
    pub prompt: Prompt,
}

#[derive(Clone)]
pub struct Loaded<'a> {
    pub general_color: Color,
    pub greeting_message: ANSIGenericString<'a, str>,
    pub greeting_color: Color,
    pub prompt: String,
    pub prompt_style: Color,
}

impl Default for Greeting {
    fn default() -> Self {
        Self {
            greeting_message: "Welcome to Calc %version% by %author%, type help for help"
                .to_string(),
            greeting_color: "blue".to_string(),
        }
    }
}

impl Default for Prompt {
    fn default() -> Self {
        Self {
            prompt: "> ".to_string(),
            prompt_color: "cyan".to_string(),
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            general_color: "purple".to_string(),
            greeting: Greeting::default(),
            prompt: Prompt::default(),
        }
    }
}

pub fn load() -> Result<Config, confy::ConfyError> {
    let cfg: Config = confy::load("mini-calc", Some("mini-calc"))?;
    Ok(cfg)
}

pub fn write_config(c: &Config) -> Result<(), ConfyError> {
    confy::store("mini-calc", Some("mini-calc"), c)?;
    Ok(())
}

pub fn write_default_config() -> Result<(), ConfyError> {
    write_config(&Config::default())
}

pub fn load_rgb_color(str: &str) -> (u8, u8, u8) {
    let first = &str[0..2];
    let second = &str[2..4];
    let last = &str[4..6];

    let rd = u8::from_str_radix(first, 16);
    let gd = u8::from_str_radix(second, 16);
    let bd = u8::from_str_radix(last, 16);

    let r = match rd {
        Ok(c) => c,
        Err(_) => 0xFF,
    };

    let g = match gd {
        Ok(c) => c,
        Err(_) => 0xFF,
    };

    let b = match bd {
        Ok(c) => c,
        Err(_) => 0xFF,
    };
    (r, g, b)
}

pub fn load_color(string: String) -> Color {
    match string.to_lowercase().as_str() {
        "purple" => Color::Purple,
        "cyan" => Color::Cyan,
        "blue" => Color::Blue,
        "black" => Color::Black,
        "red" => Color::Red,
        "yellow" => Color::Yellow,
        "green" => Color::Green,
        "white" => Color::White,
        s => {
            if s.starts_with("#") {
                let str = s.strip_prefix("#");
                if str.unwrap().len() < 6 {
                    Color::Cyan
                } else {
                    let (r, g, b) = load_rgb_color(&str.unwrap());
                    if r == 0xFF && g == 0xFF && b == 0xFF {
                        Color::Cyan
                    } else {
                        Color::RGB(r, g, b)
                    }
                }
            } else {
                Color::Cyan
            }
        }
    }
}

pub fn replace_variable(str: String) -> String {
    str.replace("%author%", "Charlotte Thomas")
        .replace("%version%", "v2.14.0")
        .to_string()
}

pub fn load_config<'a>(config: Config) -> Loaded<'a> {
    Loaded {
        greeting_color: load_color(config.clone().greeting.greeting_color),
        general_color: load_color(config.general_color),
        greeting_message: load_color(config.greeting.greeting_color)
            .paint(replace_variable(config.greeting.greeting_message)),
        prompt: config.prompt.prompt,
        prompt_style: load_color(config.prompt.prompt_color),
    }
}
