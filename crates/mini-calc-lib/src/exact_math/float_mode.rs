use std::fmt::Display;

/// # FloatMode
/// Enum describing the current floating point representation mode
/// It can either be:
/// - Normal: 5 decimal points
/// - Science: X.YYYYY * 10^Z
/// - Exact: displayed as a rational number (when possible)
#[derive(Debug, Clone)]
pub enum FloatMode {
    Normal,
    Science,
    Exact,
}

/// # Display
/// Implementation of the Display trait for FloatMode
impl Display for FloatMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal => write!(f, "normal"),
            Self::Exact => write!(f, "exact/rational"),
            Self::Science => write!(f, "science/scientific"),
        }
    }
}
