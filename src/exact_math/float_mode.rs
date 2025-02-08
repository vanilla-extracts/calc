pub enum FloatMode {
    Normal,
    Science,
    Exact,
}

impl Default for FloatMode {
    fn default() -> Self {
        Self::Normal
    }
}

impl FloatMode {
    pub fn transform(&self, f: f64) -> String {
        match self {
            Self::Science => format!("{f}"),
            _ => format!("{:.10}", f),
        }
    }
}
