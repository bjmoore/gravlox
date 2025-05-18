use std::error::Error;
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum GravloxError {
    RuntimeError(String),
}

impl Error for GravloxError {}

impl Display for GravloxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GravloxError::RuntimeError(msg) => writeln!(f, "{}", msg),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("Unexpected characer: {0}")]
    UnexpectedCharacter(char),
    #[error("Unterminated string.")]
    UnterminatedString,
    #[error("Unexpected token: {0}")]
    UnexpectedToken(&'static str),
    #[error("Too many constants in a single chunk.")]
    TooManyConstants,
    #[error("Too many local variables in current scope.")]
    TooManyLocals,
    #[error("Too much code to jump")]
    JumpTooLong,
    #[error("Loop body too long.")]
    LoopTooLong,
    #[error("Invalid assignment target.")]
    InvalidAssignment,
    #[error("Expected expression here.")]
    ExpectedExpression,
    #[error("Variable already defined in this scope.")]
    AlreadyDefined,
}

#[derive(Error, Debug)]
pub enum RuntimeError {}
