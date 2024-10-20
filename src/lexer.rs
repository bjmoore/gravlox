use crate::error::GravloxError;
use crate::token::Token;
use crate::token::TokenType;
pub struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: u32,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn lexeme(&self, token: Token) -> &str {
        println!("{:?}", token);
        &self.source[token.start..(token.start + token.len)]
    }

    pub fn next_token(&mut self) -> Result<Token, GravloxError> {
        self.start = self.current;
        self.skip_whitespace();

        if self.is_end() {
            return self.make_token(TokenType::Eof);
        }

        match self.next_char() {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                if self.match_next('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_next('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.match_next('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_next('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek_char() {
                ' ' | '\r' | '\t' => {
                    let _ = self.next_char();
                }
                '\n' => {
                    self.line += 1;
                    let _ = self.next_char();
                }
                '/' => {
                    if self.peek_next_char() == '/' {
                        while self.peek_char() != '\n' && !self.is_end() {
                            let _ = self.next_char();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn is_end(&self) -> bool {
        self.current == self.source.as_bytes().len()
    }

    fn next_char(&mut self) -> char {
        self.current += 1;
        self.source.as_bytes()[self.current - 1] as char
    }

    fn peek_char(&self) -> char {
        if self.is_end() {
            return '\0';
        }
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next_char(&self) -> char {
        if self.is_end() {
            return '\0';
        }
        self.source.as_bytes()[self.current + 1] as char
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_end() {
            return false;
        }
        if self.source.as_bytes()[self.current] as char != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn make_token(&self, token_type: TokenType) -> Result<Token, GravloxError> {
        Ok(Token::new(
            token_type,
            self.start,
            self.current - self.start,
            self.line,
        ))
    }

    fn error_token(&self, message: &'static str) -> Result<Token, GravloxError> {
        Err(GravloxError::CompileError(message))
    }

    fn string(&mut self) -> Result<Token, GravloxError> {
        while self.peek_char() != '"' && !self.is_end() {
            let next_char = self.next_char();
            if next_char == '\n' {
                self.line += 1;
            }
        }

        if self.is_end() {
            return self.error_token("Unterminated string.");
        }

        // Consume the closing quote.
        let _ = self.next_char();
        self.make_token(TokenType::String)
    }

    fn number(&mut self) -> Result<Token, GravloxError> {
        while self.peek_char().is_ascii_digit() {
            let _ = self.next_char();
        }

        if self.peek_char() == '.' && self.peek_next_char().is_ascii_digit() {
            let _ = self.next_char();

            while self.peek_char().is_ascii_digit() {
                let _ = self.next_char();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Result<Token, GravloxError> {
        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            let _ = self.next_char();
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        match self.peek_char() {
            'a' => self.keyword(1, 2, "nd", TokenType::And),
            'c' => self.keyword(1, 4, "lass", TokenType::Class),
            'e' => self.keyword(1, 3, "lse", TokenType::Else),
            'f' => {
                if self.current - self.start > 1 {
                    match self.peek_next_char() {
                        'a' => self.keyword(2, 3, "lse", TokenType::False),
                        'o' => self.keyword(2, 1, "r", TokenType::For),
                        'u' => self.keyword(2, 1, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'i' => self.keyword(1, 1, "f", TokenType::If),
            'n' => self.keyword(1, 2, "il", TokenType::Nil),
            'o' => self.keyword(1, 1, "r", TokenType::Or),
            'p' => self.keyword(1, 4, "rint", TokenType::Print),
            'r' => self.keyword(1, 5, "eturn", TokenType::Return),
            's' => self.keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.current - self.start > 1 {
                    match self.peek_next_char() {
                        'h' => self.keyword(2, 2, "is", TokenType::This),
                        'r' => self.keyword(2, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'v' => self.keyword(1, 2, "ar", TokenType::Var),
            'w' => self.keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn keyword(&self, start: usize, len: usize, rest: &str, t: TokenType) -> TokenType {
        if self.current - self.start == start + len
            && self.source.as_bytes()[self.start..len] == *rest.as_bytes()
        {
            return t;
        }

        TokenType::Identifier
    }
}
