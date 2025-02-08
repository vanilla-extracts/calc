#[derive(Debug, Clone, PartialEq)]
pub enum FloatMode {
    ExactMode,
    ScienceMode,
    NormalMode,
}

impl Default for FloatMode {
    fn default() -> Self {
        Self::NormalMode
    }
}

impl FloatMode {
    pub fn transform(&self, p: f64) -> String {
        match self {
            Self::NormalMode => format!("{:.10}", p),
            Self::ExactMode => {
                format!("")
            }
            Self::ScienceMode => {
                format!("")
            }
        }
    }
}
