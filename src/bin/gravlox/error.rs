use thiserror::Error;

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
    #[error("Too many captured variables in current scope.")]
    TooManyUpvalues,
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
    #[error("Can't return from top level.")]
    TopLevelReturn,
    #[error("Can't assign to const identifier")]
    AssignToConst
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Stack overflow.")]
    StackOverflow,
    #[error("Argument count mismatch: expected {expected} but got {actual}")]
    ArityMismatch { expected: usize, actual: usize },
    #[error("Non-callable value")]
    NonCallableValue,
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("{0} operands must be type: {1}")]
    TypeError(&'static str, &'static str),
    #[error("Can't assign to const identifier: {0}")]
    AssignToConst(String),
}
