#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
    Const,
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

    Eof,

    #[default]
    Null,
}

#[derive(Debug, Clone, Copy, Default)]
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
}
