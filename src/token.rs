#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub t: TokenType,
    pub start: usize,
    pub len: usize,
    pub line: u32,
}

impl Token {
    pub fn new(t: TokenType, start: usize, len: usize, line: u32) -> Self {
        Self {
            t,
            start,
            len,
            line,
        }
    }

    pub fn empty() -> Self {
        Self {
            t: TokenType::Eof,
            start: 0,
            len: 0,
            line: 0,
        }
    }
}
