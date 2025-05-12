use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum GravloxError {
    CompileError(&'static str),
    RuntimeError(String),
}

impl Error for GravloxError {}

impl Display for GravloxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GravloxError::CompileError(msg) => writeln!(f, "{}", msg),
            GravloxError::RuntimeError(msg) => writeln!(f, "{}", msg),
        }
    }
}
