use std::cell::RefCell;
use std::cell::RefMut;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::chunk::ChunkPtr;
use crate::error::GravloxError;
use crate::lexer::Scanner;
use crate::op::*;
use crate::token::Token;
use crate::token::TokenType;
use crate::value::FunctionPtr;
use crate::value::Value;

pub fn compile(source: String, debug: bool) -> Option<FunctionPtr> {
    let mut parser = Parser {
        current: Token::default(),
        previous: Token::default(),
        lexer: Scanner::new(source),
        had_error: false,
        panic_mode: false,
        function: crate::value::new_function("main"),
        function_type: FunctionType::Script,
        locals: [Local::default(); MAX_LOCALS],
        local_count: 0,
        scope_depth: 0,
    };

    parser.advance();
    while !parser.r#match(TokenType::Eof) {
        declaration(&mut parser);
    }
    let func = parser.end_compiler(debug);

    // Return an error here
    if !parser.had_error {
        Some(func)
    } else {
        None
    }
}

const MAX_LOCALS: usize = 256;

#[derive(Debug, Default, Clone, Copy)]
struct Local {
    name: Token,
    depth: usize,
}

enum FunctionType {
    Function,
    Script,
}

struct Parser {
    current: Token,
    previous: Token,
    lexer: Scanner,
    had_error: bool,
    panic_mode: bool,
    function: FunctionPtr,
    function_type: FunctionType,
    locals: [Local; MAX_LOCALS],
    local_count: usize,
    scope_depth: usize,
}

impl Parser {
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

    fn synchronize(&mut self) {
        self.panic_mode = false;

        loop {
            if self.current.t == TokenType::Eof {
                return;
            }

            if self.previous.t == TokenType::Semicolon {
                return;
            }

            match self.current.t {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => (),
            }

            self.advance();
        }
    }

    fn r#match(&mut self, t: TokenType) -> bool {
        if self.current.t == t {
            self.advance();
            return true;
        }

        false
    }

    fn consume(&mut self, t: TokenType, message: &'static str) {
        if self.current.t == t {
            self.advance();
        } else {
            self.error_at(self.current, GravloxError::CompileError(message));
        }
    }

    fn end_compiler(&mut self, debug: bool) -> FunctionPtr {
        self.emit_return();
        let func = self.function.clone();

        if !self.had_error && debug {
            print!("{}", self.current_chunk().borrow());
        }

        func
    }

    fn current_chunk(&self) -> ChunkPtr {
        // Return a (mutable!) object that implements add_code, add_constant, etc
        // clone the rc:: into a new container type (FunctionRef?)
        // implement pass-thru methods on FunctionRef
        self.function.borrow().chunk()
    }

    fn emit_byte(&mut self, byte: u8) {
        let line_number = self.previous.line;
        self.current_chunk()
            .borrow_mut()
            .add_code(byte, line_number);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        let line_number = self.previous.line;
        self.current_chunk()
            .borrow_mut()
            .add_code(byte1, line_number);
        self.current_chunk()
            .borrow_mut()
            .add_code(byte2, line_number);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OP_RETURN);
    }

    fn emit_constant(&mut self, value: Value) -> usize {
        let line_number = self.previous.line;
        let const_idx = self
            .current_chunk()
            .borrow_mut()
            .add_constant(value, line_number)
            .unwrap_or_else(|e| {
                self.error_at(self.previous, e);
                0
            });

        if const_idx < 256 {
            self.current_chunk()
                .borrow_mut()
                .add_code(OP_CONSTANT, line_number);
            self.current_chunk()
                .borrow_mut()
                .add_code(const_idx as u8, line_number);
        } else {
            self.current_chunk()
                .borrow_mut()
                .add_code(OP_CONSTANT_LONG, line_number);
            self.current_chunk()
                .borrow_mut()
                .add_code((const_idx >> 16) as u8, line_number);
            self.current_chunk()
                .borrow_mut()
                .add_code((const_idx >> 8) as u8, line_number);
            self.current_chunk()
                .borrow_mut()
                .add_code(const_idx as u8, line_number);
        }

        const_idx
    }

    // Push these down into the Chunk impl.
    fn emit_jump(&mut self, instruction: u8) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.current_chunk().borrow().count() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().borrow().count() - offset - 2;

        if jump > u16::MAX.into() {
            self.error_at(
                self.previous,
                GravloxError::CompileError("Too much code to jump."),
            );
        }

        self.current_chunk()
            .borrow_mut()
            .patch_byte(offset, (jump >> 8) as u8);
        self.current_chunk()
            .borrow_mut()
            .patch_byte(offset + 1, jump as u8);
    }

    fn emit_loop(&mut self, location: usize) {
        self.emit_byte(OP_LOOP);

        let offset = self.current_chunk().borrow().count() - location + 2; // +2 because while processing this jump, we will advance ip by 2
        if offset > u16::MAX.into() {
            self.error_at(
                self.previous,
                GravloxError::CompileError("Loop body too long."),
            );
        }

        self.emit_byte((offset >> 8) as u8);
        self.emit_byte(offset as u8);
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

    fn lexeme(&self) -> &str {
        self.lexer.lexeme(self.previous)
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
            self.emit_byte(OP_POP);
            self.local_count -= 1;
        }
    }

    fn add_local(&mut self, name: Token) {
        if self.local_count == MAX_LOCALS {
            self.error_at(
                self.previous,
                GravloxError::CompileError("Too many local variables"),
            );
            return;
        }

        let local = &mut self.locals[self.local_count];

        local.name = name;
        local.depth = self.scope_depth;

        self.local_count += 1;
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

    let assignable = precedence <= Precedence::Assignment;
    prefix_rule(parser, assignable);

    while precedence < get_rule(parser.current.t).2 {
        parser.advance();
        let infix_rule = get_rule(parser.previous.t).1;
        match infix_rule {
            Some(mut rule) => rule(parser, assignable),
            None => (),
        }
    }

    if assignable && parser.r#match(TokenType::Equal) {
        parser.error_at(
            parser.previous,
            GravloxError::CompileError("Invalid assignment target"),
        );
    }
}

fn declaration(parser: &mut Parser) {
    if parser.r#match(TokenType::Var) {
        var_declaration(parser);
    } else {
        statement(parser);
    }

    if parser.panic_mode {
        parser.synchronize();
    }
}

fn var_declaration(parser: &mut Parser) {
    let global = parse_variable(parser, "Expect variable name.");

    if parser.r#match(TokenType::Equal) {
        expression(parser);
    } else {
        parser.emit_byte(OP_NIL);
    }

    parser.consume(
        TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    );

    define_variable(parser, global);
}

fn parse_variable(parser: &mut Parser, message: &'static str) -> usize {
    parser.consume(TokenType::Identifier, message);

    declare_variable(parser);
    if parser.scope_depth > 0 {
        return 0;
    }

    identifier_constant(parser)
}

fn declare_variable(parser: &mut Parser) {
    if parser.scope_depth == 0 {
        return;
    }

    let name = parser.previous;
    for i in (0..parser.local_count).rev() {
        let local = parser.locals[i];
        if local.depth < parser.scope_depth {
            break;
        }

        if identifers_equal(parser, name, local.name) {
            // TODO: error here
            parser.error_at(
                parser.previous,
                GravloxError::CompileError("Variable already defined in this scope."),
            );
        }
    }
    parser.add_local(name);
}

fn identifier_constant(parser: &mut Parser) -> usize {
    let name = parser.lexeme();
    let heap_obj = Rc::new(RefCell::new(name.to_owned()));
    let line = parser.previous.line;
    parser
        .current_chunk()
        .borrow_mut()
        .add_constant(Value::StringRef(heap_obj), line)
        .unwrap_or_else(|e| {
            parser.error_at(parser.previous, e);
            0
        })
}

fn identifers_equal(parser: &Parser, a: Token, b: Token) -> bool {
    if a.len != b.len {
        return false;
    }
    parser.lexer.lexeme(a) == parser.lexer.lexeme(b)
}

fn define_variable(parser: &mut Parser, global: usize) {
    if parser.scope_depth > 0 {
        return;
    }

    // TODO: Since we support 24-bit constants with OP_CONSTANT_LONG, we should have an OP_DEFINE_GLOBAL_LONG
    parser.emit_bytes(OP_DEFINE_GLOBAL, global as u8);
}

fn and(parser: &mut Parser, _assignable: bool) {
    let end_jump = parser.emit_jump(OP_JUMP_IF_FALSE);

    parser.emit_byte(OP_POP);
    parse_precedence(parser, Precedence::And);

    parser.patch_jump(end_jump);
}

fn or(parser: &mut Parser, _assignable: bool) {
    let else_jump = parser.emit_jump(OP_JUMP_IF_FALSE);
    let end_jump = parser.emit_jump(OP_JUMP);

    parser.patch_jump(else_jump);

    parser.emit_byte(OP_POP);
    parse_precedence(parser, Precedence::Or);

    parser.patch_jump(end_jump);
}

fn statement(parser: &mut Parser) {
    if parser.r#match(TokenType::Print) {
        print_statement(parser);
    } else if parser.r#match(TokenType::LeftBrace) {
        parser.begin_scope();
        block(parser);
        parser.end_scope();
    } else if parser.r#match(TokenType::If) {
        if_statement(parser);
    } else if parser.r#match(TokenType::While) {
        while_statement(parser);
    } else if parser.r#match(TokenType::For) {
        for_statement(parser);
    } else {
        expression_statement(parser);
    }
}

fn print_statement(parser: &mut Parser) {
    expression(parser);
    parser.consume(TokenType::Semicolon, "Expect ';' after value.");
    parser.emit_byte(OP_PRINT);
}

fn if_statement(parser: &mut Parser) {
    parser.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
    expression(parser);
    parser.consume(TokenType::RightParen, "Expect ')' after condition.");

    let then_jump = parser.emit_jump(OP_JUMP_IF_FALSE);

    parser.emit_byte(OP_POP);
    statement(parser);

    let else_jump = parser.emit_jump(OP_JUMP);

    parser.patch_jump(then_jump);

    parser.emit_byte(OP_POP);
    if parser.r#match(TokenType::Else) {
        statement(parser);
    }

    parser.patch_jump(else_jump);
}

fn while_statement(parser: &mut Parser) {
    let loop_start = parser.current_chunk().borrow().count();

    parser.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
    expression(parser);
    parser.consume(TokenType::RightParen, "Expect ')' after condition.");

    let exit_loop = parser.emit_jump(OP_JUMP_IF_FALSE);
    parser.emit_byte(OP_POP);

    statement(parser);
    parser.emit_loop(loop_start);

    parser.patch_jump(exit_loop);
    parser.emit_byte(OP_POP);
}

fn for_statement(parser: &mut Parser) {
    parser.begin_scope();
    parser.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

    // initializer
    if parser.r#match(TokenType::Semicolon) {
        // No initializer.
    } else if parser.r#match(TokenType::Var) {
        var_declaration(parser);
    } else {
        expression_statement(parser);
    }

    let mut loop_start = parser.current_chunk().borrow().count();
    let mut exit_jump = None;

    // condition
    if !parser.r#match(TokenType::Semicolon) {
        expression(parser);
        parser.consume(TokenType::Semicolon, "Expect ';' after for loop condition.");

        exit_jump = Some(parser.emit_jump(OP_JUMP_IF_FALSE));
        parser.emit_byte(OP_POP);
    }

    // increment
    if !parser.r#match(TokenType::RightParen) {
        let body_jump = parser.emit_jump(OP_JUMP);
        let increment_start = parser.current_chunk().borrow().count();
        expression(parser);
        parser.emit_byte(OP_POP);
        parser.consume(
            TokenType::RightParen,
            "Expect ')' after for loop increment.",
        );
        parser.emit_loop(loop_start);
        loop_start = increment_start;
        parser.patch_jump(body_jump);
    }

    statement(parser);

    parser.emit_loop(loop_start);
    if let Some(exit_jump) = exit_jump {
        parser.patch_jump(exit_jump);
    }
    parser.emit_byte(OP_POP);
    parser.end_scope();
}

fn expression_statement(parser: &mut Parser) {
    expression(parser);
    parser.consume(TokenType::Semicolon, "Expect ';' after expression.");
    parser.emit_byte(OP_POP);
}

fn block(parser: &mut Parser) {
    while !(parser.current.t == TokenType::RightBrace) && !(parser.current.t == TokenType::Eof) {
        declaration(parser);
    }

    parser.consume(TokenType::RightBrace, "Expect '}' after block.");
}

fn expression(parser: &mut Parser) {
    parse_precedence(parser, Precedence::Assignment);
}

fn unary(parser: &mut Parser, _assignable: bool) {
    let operator_type = parser.previous.t;

    parse_precedence(parser, Precedence::Unary);

    match operator_type {
        TokenType::Bang => parser.emit_byte(OP_NOT),
        TokenType::Minus => parser.emit_byte(OP_NEGATE),
        _ => unreachable!(),
    }
}

fn binary(parser: &mut Parser, _assignable: bool) {
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

fn grouping(parser: &mut Parser, _assignable: bool) {
    expression(parser);
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn number(parser: &mut Parser, _assignable: bool) {
    let lexeme = parser.lexer.lexeme(parser.previous);
    let number = lexeme.parse::<f64>().unwrap();
    parser.emit_constant(Value::Number(number));
}

fn literal(parser: &mut Parser, _assignable: bool) {
    let operator_type = parser.previous.t;

    match operator_type {
        TokenType::Nil => parser.emit_byte(OP_NIL),
        TokenType::True => parser.emit_byte(OP_TRUE),
        TokenType::False => parser.emit_byte(OP_FALSE),
        _ => unreachable!(),
    }
}

fn string(parser: &mut Parser, _assignable: bool) {
    let str_value = parser.lexer.string_lexeme(parser.previous);
    let heap_obj = Rc::new(RefCell::new(str_value.to_owned()));
    parser.emit_constant(Value::StringRef(heap_obj));
}

fn variable(parser: &mut Parser, assignable: bool) {
    named_variable(parser, assignable);
}

fn named_variable(parser: &mut Parser, assignable: bool) {
    let (get_op, set_op, arg) = match resolve_local(parser) {
        Some(arg) => (OP_GET_LOCAL, OP_SET_LOCAL, arg),
        None => {
            let arg = identifier_constant(parser) as u8;
            (OP_GET_GLOBAL, OP_SET_GLOBAL, arg)
        }
    };

    if assignable && parser.r#match(TokenType::Equal) {
        expression(parser);
        parser.emit_bytes(set_op, arg);
    } else {
        parser.emit_bytes(get_op, arg);
    }
}

fn resolve_local(parser: &mut Parser) -> Option<u8> {
    for i in (0..parser.local_count).rev() {
        let local = parser.locals[i];
        if identifers_equal(parser, local.name, parser.previous) {
            return Some(i as u8);
        }
    }

    None
}

struct ParseRule(
    Option<Box<dyn FnMut(&mut Parser, bool)>>,
    Option<Box<dyn FnMut(&mut Parser, bool)>>,
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
        TokenType::Identifier   => ParseRule(Some(Box::new(variable)), None,                   Precedence::None),
        TokenType::String       => ParseRule(Some(Box::new(string)),   None,                   Precedence::None),
        TokenType::Number       => ParseRule(Some(Box::new(number)),   None,                   Precedence::None),
        TokenType::And          => ParseRule(None,                     Some(Box::new(and)),    Precedence::And),
        TokenType::Class        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Else         => ParseRule(None,                     None,                   Precedence::None),
        TokenType::False        => ParseRule(Some(Box::new(literal)),  None,                   Precedence::None),
        TokenType::For          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Fun          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::If           => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Nil          => ParseRule(Some(Box::new(literal)),  None,                   Precedence::None),
        TokenType::Or           => ParseRule(None,                     Some(Box::new(or)),     Precedence::Or),
        TokenType::Print        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Return       => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Super        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::This         => ParseRule(None,                     None,                   Precedence::None),
        TokenType::True         => ParseRule(Some(Box::new(literal)),  None,                   Precedence::None),
        TokenType::Var          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::While        => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Eof          => ParseRule(None,                     None,                   Precedence::None),
        TokenType::Null         => unreachable!("ERROR: Attempted to parse a null token"),
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
