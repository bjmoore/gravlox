use std::cell::RefCell;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::error::GravloxError;
use crate::lexer::Scanner;
use crate::op::*;
use crate::token::Token;
use crate::token::TokenType;
use crate::value::Obj;
use crate::value::Value;

pub fn compile(source: String, chunk: &mut Chunk, debug: bool) -> bool {
    let mut parser = Parser {
        current: Token::empty(),
        previous: Token::empty(),
        lexer: Scanner::new(source),
        had_error: false,
        panic_mode: false,
        compiling_chunk: chunk,
        heap: Vec::new(),
    };

    parser.advance();
    expression(&mut parser);
    parser.consume(TokenType::Eof, "Expect end of expression.");
    parser.end_compiler(debug);

    // Return heap objects here
    !parser.had_error
}

struct Parser<'a> {
    current: Token,
    previous: Token,
    lexer: Scanner,
    had_error: bool,
    panic_mode: bool,
    compiling_chunk: &'a mut Chunk,
    heap: Vec<Rc<RefCell<Obj>>>,
}

impl<'a> Parser<'a> {
    fn advance(&mut self) {
        self.previous = self.current;
        loop {
            match self.lexer.next_token() {
                Ok(token) => {
                    self.current = token;
                    break;
                }
                Err(err) => {
                    self.error_at(self.current, err);
                }
            }
        }
    }

    fn consume(&mut self, t: TokenType, message: &'static str) {
        if self.current.t == t {
            self.advance();
        } else {
            self.error_at(self.current, GravloxError::CompileError(message));
        }
    }

    fn end_compiler(&mut self, debug: bool) {
        self.emit_return();
        if !self.had_error && debug {
            print!("{}", self.current_chunk());
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.compiling_chunk
    }

    fn emit_byte(&mut self, byte: u8) {
        let line_number = self.previous.line;
        self.current_chunk().add_code(byte, line_number);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        let line_number = self.previous.line;
        self.current_chunk().add_code(byte1, line_number);
        self.current_chunk().add_code(byte2, line_number);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OP_RETURN);
    }

    fn emit_constant(&mut self, value: Value) {
        let line_number = self.previous.line;
        self.current_chunk()
            .add_constant(value, line_number)
            .unwrap_or_else(|e| self.error_at(self.previous, e));
    }

    fn error_at(&mut self, token: Token, err: GravloxError) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        if token.t == TokenType::Eof {
            println!("[line {}]: Error at end: {}", token.line, err);
        } else {
            println!(
                "[line {}]: Error at '{}': {}",
                token.line,
                self.lexer.lexeme(token),
                err
            );
        }
        self.had_error = true;
    }

    fn heap_add(&mut self, obj: Rc<RefCell<Obj>>) {
        self.heap.push(obj);
    }
}

fn parse_precedence(parser: &mut Parser, precedence: Precedence) {
    parser.advance();
    let mut prefix_rule = match get_rule(parser.previous.t).0 {
        Some(rule) => rule,
        None => {
            parser.error_at(
                parser.previous,
                GravloxError::CompileError("Expect expression."),
            );
            return;
        }
    };

    prefix_rule(parser);

    while precedence < get_rule(parser.current.t).2 {
        parser.advance();
        let infix_rule = get_rule(parser.previous.t).1;
        match infix_rule {
            Some(mut rule) => rule(parser),
            None => (),
        }
    }
}

fn expression(parser: &mut Parser) {
    parse_precedence(parser, Precedence::Assignment);
}

fn unary(parser: &mut Parser) {
    let operator_type = parser.previous.t;

    parse_precedence(parser, Precedence::Unary);

    match operator_type {
        TokenType::Bang => parser.emit_byte(OP_NOT),
        TokenType::Minus => parser.emit_byte(OP_NEGATE),
        _ => unreachable!(),
    }
}

fn binary(parser: &mut Parser) {
    let operator_type = parser.previous.t;
    let parse_rule = get_rule(operator_type);
    parse_precedence(parser, parse_rule.2.plus_one());

    match operator_type {
        TokenType::Plus => parser.emit_byte(OP_ADD),
        TokenType::Minus => parser.emit_byte(OP_SUBTRACT),
        TokenType::Star => parser.emit_byte(OP_MULTIPLY),
        TokenType::Slash => parser.emit_byte(OP_DIVIDE),
        TokenType::BangEqual => parser.emit_bytes(OP_EQUAL, OP_NOT),
        TokenType::EqualEqual => parser.emit_byte(OP_EQUAL),
        TokenType::Greater => parser.emit_byte(OP_GREATER),
        TokenType::GreaterEqual => parser.emit_bytes(OP_GREATER, OP_NOT),
        TokenType::Less => parser.emit_byte(OP_LESS),
        TokenType::LessEqual => parser.emit_bytes(OP_LESS, OP_NOT),
        _ => unreachable!(),
    }
}

fn grouping(parser: &mut Parser) {
    expression(parser);
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn number(parser: &mut Parser) {
    let lexeme = parser.lexer.lexeme(parser.previous);
    let number = lexeme.parse::<f64>().unwrap();
    parser.emit_constant(Value::Number(number));
}

fn literal(parser: &mut Parser) {
    let operator_type = parser.previous.t;

    match operator_type {
        TokenType::Nil => parser.emit_byte(OP_NIL),
        TokenType::True => parser.emit_byte(OP_TRUE),
        TokenType::False => parser.emit_byte(OP_FALSE),
        _ => unreachable!(),
    }
}

fn string(parser: &mut Parser) {
    let str_value = parser.lexer.lexeme(parser.previous);
    let heap_obj = Rc::new(RefCell::new(Obj::String(str_value.to_owned())));
    parser.heap_add(heap_obj.clone());
    parser.emit_constant(Value::ObjRef(heap_obj));
}

struct ParseRule(
    Option<Box<dyn FnMut(&mut Parser)>>,
    Option<Box<dyn FnMut(&mut Parser)>>,
    Precedence,
);

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

fn get_rule(t: TokenType) -> ParseRule {
    #[rustfmt::skip]
    return match t { // Using return here because #[rustfmt::skip] on an expression is an error
        TokenType::LeftParen    => ParseRule(Some(Box::new(grouping)), None,                   Precedence::None),
        TokenType::RightParen   => ParseRule(None,                     None,                   Precedence::None),
        TokenType::LeftBrace    => ParseRule(None,                     None,                   Precedence::None),
        TokenType::RightBrace   => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Comma        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Dot          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Minus        => ParseRule(Some(Box::new(unary)),    Some(Box::new(binary)), Precedence::Term),
        TokenType::Plus         => ParseRule(None,                     Some(Box::new(binary)), Precedence::Term),
        TokenType::Semicolon    => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Slash        => ParseRule(None,                     Some(Box::new(binary)), Precedence::Factor),
        TokenType::Star         => ParseRule(None,                     Some(Box::new(binary)), Precedence::Factor),
        TokenType::Bang         => ParseRule(Some(Box::new(unary)),    None,                   Precedence::None),
        TokenType::BangEqual    => ParseRule(None,                     Some(Box::new(binary)), Precedence::Equality),
        TokenType::Equal        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::EqualEqual   => ParseRule(None,                     Some(Box::new(binary)), Precedence::Equality),
        TokenType::Greater      => ParseRule(None,                     Some(Box::new(binary)), Precedence::Comparison),
        TokenType::GreaterEqual => ParseRule(None,                     Some(Box::new(binary)), Precedence::Comparison),
        TokenType::Less         => ParseRule(None,                     Some(Box::new(binary)), Precedence::Comparison),
        TokenType::LessEqual    => ParseRule(None,                     Some(Box::new(binary)), Precedence::Comparison),
        TokenType::Identifier   => ParseRule(None,                     None,                   Precedence::None),
        TokenType::String       => ParseRule(Some(Box::new(string)),   None,                   Precedence::None),
        TokenType::Number       => ParseRule(Some(Box::new(number)),   None,                   Precedence::None),
        TokenType::And          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Class        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Else         => ParseRule(None,                     None,                   Precedence::None),
        TokenType::False        => ParseRule(Some(Box::new(literal)),  None,                   Precedence::None),
        TokenType::For          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Fun          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::If           => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Nil          => ParseRule(Some(Box::new(literal)),  None,                   Precedence::None),
        TokenType::Or           => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Print        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Return       => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Super        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::This         => ParseRule(None,                     None,                   Precedence::None),
        TokenType::True         => ParseRule(Some(Box::new(literal)),  None,                   Precedence::None),
        TokenType::Var          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::While        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Eof          => ParseRule(None,                     None,                   Precedence::None),
    };
}

impl Precedence {
    fn plus_one(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => panic!(),
        }
    }
}
