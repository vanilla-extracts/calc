use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum FloatMode {
    Normal,
    Science,
    Exact,
}

impl Display for FloatMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal => write!(f, "normal"),
            Self::Exact => write!(f, "exact/rational"),
            Self::Science => write!(f, "science/scientific"),
        }
    }
}
