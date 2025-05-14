use std::cell::RefCell;
use std::rc::Rc;

use crate::chunk::ChunkPtr;
use crate::error::GravloxError;
use crate::lexer::Scanner;
use crate::obj::{make_obj, Obj};
use crate::op::*;
use crate::take::Take;
use crate::token::Token;
use crate::token::TokenType;
use crate::value::Function;
use crate::value::Value;

pub fn compile(source: String, debug: bool) -> Option<Obj<Function>> {
    let mut parser = Parser {
        current: Token::default(),
        previous: Token::default(),
        lexer: Scanner::new(source),
        had_error: false,
        panic_mode: false,
    };

    let mut compiler = Take::new(Compiler::new(None, FunctionType::Script, Some("<root>")));

    parser.advance();
    while !parser.r#match(TokenType::Eof) {
        declaration(&mut parser, &mut compiler);
    }
    let func = compiler.end_compiler(parser.line_number(), debug);

    // Return an error here
    if !parser.had_error {
        Some(func)
    } else {
        None
    }
}

struct Compiler {
    function: Obj<Function>,
    function_type: FunctionType,
    locals: [Local; MAX_LOCALS],
    local_count: usize,
    scope_depth: usize,
    enclosing: Option<Box<Compiler>>,
}

impl Compiler {
    fn new(enclosing: Option<Compiler>, r#type: FunctionType, name: Option<&str>) -> Self {
        Self {
            function: make_obj(Function::new(name)),
            function_type: r#type,
            locals: [Local::default(); MAX_LOCALS],
            local_count: 0,
            scope_depth: 0,
            enclosing: enclosing.map(|c| Box::new(c)),
        }
    }

    fn end_compiler(&mut self, line_number: u32, _debug: bool) -> Obj<Function> {
        self.emit_return(line_number);
        let func = self.function.clone();

        func
    }

    fn current_chunk(&self) -> ChunkPtr {
        // Return a (mutable!) object that implements add_code, add_constant, etc
        // clone the rc:: into a new container type (FunctionRef?)
        // implement pass-thru methods on FunctionRef
        self.function.borrow().chunk()
    }

    fn emit_byte(&mut self, byte: u8, line_number: u32) {
        self.current_chunk()
            .borrow_mut()
            .add_code(byte, line_number);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8, line_number: u32) {
        self.current_chunk()
            .borrow_mut()
            .add_code(byte1, line_number);
        self.current_chunk()
            .borrow_mut()
            .add_code(byte2, line_number);
    }

    fn emit_return(&mut self, line_number: u32) {
        self.emit_byte(OP_RETURN, line_number);
    }

    fn emit_constant(&mut self, value: Value, line_number: u32) -> Result<usize, GravloxError> {
        let const_idx = self
            .current_chunk()
            .borrow_mut()
            .add_constant(value, line_number)?;

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

        Ok(const_idx)
    }

    fn emit_jump(&mut self, instruction: u8, line_number: u32) -> usize {
        self.emit_byte(instruction, line_number);
        self.emit_byte(0xFF, line_number);
        self.emit_byte(0xFF, line_number);
        self.current_chunk().borrow().count() - 2
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), GravloxError> {
        let jump = self.current_chunk().borrow().count() - offset - 2;

        if jump > u16::MAX.into() {
            return Err(GravloxError::CompileError("Too much code to jump."));
        }

        self.current_chunk()
            .borrow_mut()
            .patch_byte(offset, (jump >> 8) as u8);
        self.current_chunk()
            .borrow_mut()
            .patch_byte(offset + 1, jump as u8);

        Ok(())
    }

    fn emit_loop(&mut self, location: usize, line_number: u32) -> Result<(), GravloxError> {
        self.emit_byte(OP_LOOP, line_number);

        let offset = self.current_chunk().borrow().count() - location + 2; // +2 because while processing this jump, we will advance ip by 2
        if offset > u16::MAX.into() {
            return Err(GravloxError::CompileError("Loop body too long."));
        }

        self.emit_byte((offset >> 8) as u8, line_number);
        self.emit_byte(offset as u8, line_number);

        Ok(())
    }

    fn add_local(&mut self, name: Token) -> Result<(), GravloxError> {
        if self.local_count == MAX_LOCALS {
            return Err(GravloxError::CompileError("Too many local variables"));
        }

        let local = &mut self.locals[self.local_count];

        local.name = name;
        local.depth = None;

        self.local_count += 1;

        Ok(())
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        self.locals[self.local_count - 1].depth = Some(self.scope_depth);
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, line_number: u32) {
        self.scope_depth -= 1;

        while self.local_count > 0
            && self.locals[self.local_count - 1].depth.unwrap() > self.scope_depth
        {
            self.emit_byte(OP_POP, line_number);
            self.local_count -= 1;
        }
    }
}

const MAX_LOCALS: usize = 256;

#[derive(Debug, Default, Clone, Copy)]
struct Local {
    name: Token,
    depth: Option<usize>,
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
}

impl Parser {
    fn new(source: String) -> Self {
        Self {
            current: Token::default(),
            previous: Token::default(),
            lexer: Scanner::new(source),
            had_error: false,
            panic_mode: false,
        }
    }

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

    fn check(&self, t: TokenType) -> bool {
        self.current.t == t
    }

    fn consume(&mut self, t: TokenType, message: &'static str) {
        if self.current.t == t {
            self.advance();
        } else {
            self.error_at(self.current, GravloxError::CompileError(message));
        }
    }

    fn error(&mut self, err: GravloxError) {
        self.error_at(self.previous, err);
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

    fn line_number(&self) -> u32 {
        self.previous.line
    }
}

fn parse_precedence(parser: &mut Parser, compiler: &mut Take<Compiler>, precedence: Precedence) {
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
    prefix_rule(parser, compiler, assignable);

    while precedence < get_rule(parser.current.t).2 {
        parser.advance();
        let infix_rule = get_rule(parser.previous.t).1;
        match infix_rule {
            Some(mut rule) => rule(parser, compiler, assignable),
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

fn declaration(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    if parser.r#match(TokenType::Fun) {
        fun_declaration(parser, compiler);
    } else if parser.r#match(TokenType::Var) {
        var_declaration(parser, compiler);
    } else {
        statement(parser, compiler);
    }

    if parser.panic_mode {
        parser.synchronize();
    }
}

fn var_declaration(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    let global = parse_variable(parser, compiler, "Expect variable name.");

    if parser.r#match(TokenType::Equal) {
        expression(parser, compiler);
    } else {
        compiler.emit_byte(OP_NIL, parser.line_number());
    }

    parser.consume(
        TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    );

    define_variable(parser, compiler, global);
}

fn parse_variable(
    parser: &mut Parser,
    compiler: &mut Take<Compiler>,
    message: &'static str,
) -> usize {
    parser.consume(TokenType::Identifier, message);

    declare_variable(parser, compiler);
    if compiler.scope_depth > 0 {
        return 0;
    }

    identifier_constant(parser, compiler)
}

fn declare_variable(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    if compiler.scope_depth == 0 {
        return;
    }

    let name = parser.previous;
    for i in (0..compiler.local_count).rev() {
        let local = compiler.locals[i];
        if let Some(depth) = local.depth {
            if depth < compiler.scope_depth {
                break;
            }
        }

        if identifiers_equal(parser, name, local.name) {
            parser.error_at(
                parser.previous,
                GravloxError::CompileError("Variable already defined in this scope."),
            );
        }
    }
    compiler
        .add_local(name)
        .inspect_err(|e| parser.error(e.clone()));
}

fn identifier_constant(parser: &mut Parser, compiler: &mut Take<Compiler>) -> usize {
    let name = parser.lexeme();
    let heap_obj = Rc::new(RefCell::new(name.to_owned()));
    let line = parser.previous.line;
    compiler
        .current_chunk()
        .borrow_mut()
        .add_constant(Value::StringRef(heap_obj), line)
        .unwrap_or_else(|e| {
            parser.error_at(parser.previous, e);
            0
        })
}

fn identifiers_equal(parser: &Parser, a: Token, b: Token) -> bool {
    if a.len != b.len {
        return false;
    }
    parser.lexer.lexeme(a) == parser.lexer.lexeme(b)
}

fn define_variable(parser: &mut Parser, compiler: &mut Take<Compiler>, global: usize) {
    if compiler.scope_depth > 0 {
        compiler.mark_initialized();
        return;
    }

    // TODO: Since we support 24-bit constants with OP_CONSTANT_LONG, we should have an OP_DEFINE_GLOBAL_LONG
    compiler.emit_bytes(OP_DEFINE_GLOBAL, global as u8, parser.line_number());
}

fn and(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let end_jump = compiler.emit_jump(OP_JUMP_IF_FALSE, parser.line_number());

    compiler.emit_byte(OP_POP, parser.line_number());
    parse_precedence(parser, compiler, Precedence::And);

    compiler
        .patch_jump(end_jump)
        .inspect_err(|e| parser.error(e.clone()));
}

fn or(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let else_jump = compiler.emit_jump(OP_JUMP_IF_FALSE, parser.line_number());
    let end_jump = compiler.emit_jump(OP_JUMP, parser.line_number());

    compiler
        .patch_jump(else_jump)
        .inspect_err(|e| parser.error(e.clone()));

    compiler.emit_byte(OP_POP, parser.line_number());
    parse_precedence(parser, compiler, Precedence::Or);

    compiler
        .patch_jump(end_jump)
        .inspect_err(|e| parser.error(e.clone()));
}

fn statement(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    if parser.r#match(TokenType::Print) {
        print_statement(parser, compiler);
    } else if parser.r#match(TokenType::LeftBrace) {
        compiler.begin_scope();
        block(parser, compiler);
        compiler.end_scope(parser.line_number());
    } else if parser.r#match(TokenType::If) {
        if_statement(parser, compiler);
    } else if parser.r#match(TokenType::While) {
        while_statement(parser, compiler);
    } else if parser.r#match(TokenType::For) {
        for_statement(parser, compiler);
    } else {
        expression_statement(parser, compiler);
    }
}

fn print_statement(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    expression(parser, compiler);
    parser.consume(TokenType::Semicolon, "Expect ';' after value.");
    compiler.emit_byte(OP_PRINT, parser.line_number());
}

fn if_statement(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    parser.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
    expression(parser, compiler);
    parser.consume(TokenType::RightParen, "Expect ')' after condition.");

    let then_jump = compiler.emit_jump(OP_JUMP_IF_FALSE, parser.line_number());

    compiler.emit_byte(OP_POP, parser.line_number());
    statement(parser, compiler);

    let else_jump = compiler.emit_jump(OP_JUMP, parser.line_number());

    compiler
        .patch_jump(then_jump)
        .inspect_err(|e| parser.error(e.clone()));

    compiler.emit_byte(OP_POP, parser.line_number());
    if parser.r#match(TokenType::Else) {
        statement(parser, compiler);
    }

    compiler
        .patch_jump(else_jump)
        .inspect_err(|e| parser.error(e.clone()));
}

fn while_statement(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    let loop_start = compiler.current_chunk().borrow().count();

    parser.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
    expression(parser, compiler);
    parser.consume(TokenType::RightParen, "Expect ')' after condition.");

    let exit_loop = compiler.emit_jump(OP_JUMP_IF_FALSE, parser.line_number());
    compiler.emit_byte(OP_POP, parser.line_number());

    statement(parser, compiler);
    compiler
        .emit_loop(loop_start, parser.line_number())
        .inspect_err(|e| parser.error(e.clone()));

    compiler
        .patch_jump(exit_loop)
        .inspect_err(|e| parser.error(e.clone()));
    compiler.emit_byte(OP_POP, parser.line_number());
}

fn for_statement(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    compiler.begin_scope();
    parser.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

    // initializer
    if parser.r#match(TokenType::Semicolon) {
        // No initializer.
    } else if parser.r#match(TokenType::Var) {
        var_declaration(parser, compiler);
    } else {
        expression_statement(parser, compiler);
    }

    let mut loop_start = compiler.current_chunk().borrow().count();
    let mut exit_jump = None;

    // condition
    if !parser.r#match(TokenType::Semicolon) {
        expression(parser, compiler);
        parser.consume(TokenType::Semicolon, "Expect ';' after for loop condition.");

        exit_jump = Some(compiler.emit_jump(OP_JUMP_IF_FALSE, parser.line_number()));
        compiler.emit_byte(OP_POP, parser.line_number());
    }

    // increment
    if !parser.r#match(TokenType::RightParen) {
        let body_jump = compiler.emit_jump(OP_JUMP, parser.line_number());
        let increment_start = compiler.current_chunk().borrow().count();
        expression(parser, compiler);
        compiler.emit_byte(OP_POP, parser.line_number());
        parser.consume(
            TokenType::RightParen,
            "Expect ')' after for loop increment.",
        );
        compiler
            .emit_loop(loop_start, parser.line_number())
            .inspect_err(|e| parser.error(e.clone()));
        loop_start = increment_start;
        compiler
            .patch_jump(body_jump)
            .inspect_err(|e| parser.error(e.clone()));
    }

    statement(parser, compiler);

    compiler
        .emit_loop(loop_start, parser.line_number())
        .inspect_err(|e| parser.error(e.clone()));
    if let Some(exit_jump) = exit_jump {
        compiler
            .patch_jump(exit_jump)
            .inspect_err(|e| parser.error(e.clone()));
    }
    compiler.emit_byte(OP_POP, parser.line_number());
    compiler.end_scope(parser.line_number());
}

fn expression_statement(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    expression(parser, compiler);
    parser.consume(TokenType::Semicolon, "Expect ';' after expression.");
    compiler.emit_byte(OP_POP, parser.line_number());
}

fn block(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    while !(parser.current.t == TokenType::RightBrace) && !(parser.current.t == TokenType::Eof) {
        declaration(parser, compiler);
    }

    parser.consume(TokenType::RightBrace, "Expect '}' after block.");
}

fn fun_declaration(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    let global = parse_variable(parser, compiler, "Expect variable name.");
    compiler.mark_initialized();
    function(parser, compiler, FunctionType::Function);
    define_variable(parser, compiler, global);
}

fn function(parser: &mut Parser, compiler: &mut Take<Compiler>, function_type: FunctionType) {
    compiler.push_compiler(function_type, Some(parser.lexer.lexeme(parser.previous)));
    compiler.begin_scope();

    parser.consume(TokenType::LeftParen, "Expect '(' after function name.");
    if !parser.check(TokenType::RightParen) {
        loop {
            compiler.function.borrow_mut().arity += 1;
            if compiler.function.borrow().arity > 255 {
                // emit too many arguments error
            }
            let constant = parse_variable(parser, compiler, "Expect parameter name.");
            define_variable(parser, compiler, constant);
            if !parser.r#match(TokenType::Comma) {
                break;
            }
        }
    }
    parser.consume(TokenType::RightParen, "Expect ')' after function name.");
    parser.consume(TokenType::LeftBrace, "Expect '{' before function body.");
    block(parser, compiler);

    let func = compiler.end_compiler(parser.line_number(), false);
    compiler.pop_compiler();
    compiler.emit_constant(Value::FunctionRef(func), parser.line_number());
}

fn expression(parser: &mut Parser, compiler: &mut Take<Compiler>) {
    parse_precedence(parser, compiler, Precedence::Assignment);
}

fn unary(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let operator_type = parser.previous.t;

    parse_precedence(parser, compiler, Precedence::Unary);

    match operator_type {
        TokenType::Bang => compiler.emit_byte(OP_NOT, parser.line_number()),
        TokenType::Minus => compiler.emit_byte(OP_NEGATE, parser.line_number()),
        _ => unreachable!(),
    }
}

fn binary(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let operator_type = parser.previous.t;
    let parse_rule = get_rule(operator_type);
    parse_precedence(parser, compiler, parse_rule.2.plus_one());

    match operator_type {
        TokenType::Plus => compiler.emit_byte(OP_ADD, parser.line_number()),
        TokenType::Minus => compiler.emit_byte(OP_SUBTRACT, parser.line_number()),
        TokenType::Star => compiler.emit_byte(OP_MULTIPLY, parser.line_number()),
        TokenType::Slash => compiler.emit_byte(OP_DIVIDE, parser.line_number()),
        TokenType::BangEqual => compiler.emit_bytes(OP_EQUAL, OP_NOT, parser.line_number()),
        TokenType::EqualEqual => compiler.emit_byte(OP_EQUAL, parser.line_number()),
        TokenType::Greater => compiler.emit_byte(OP_GREATER, parser.line_number()),
        TokenType::GreaterEqual => compiler.emit_bytes(OP_GREATER, OP_NOT, parser.line_number()),
        TokenType::Less => compiler.emit_byte(OP_LESS, parser.line_number()),
        TokenType::LessEqual => compiler.emit_bytes(OP_LESS, OP_NOT, parser.line_number()),
        _ => unreachable!(),
    }
}

fn grouping(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    expression(parser, compiler);
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn number(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let lexeme = parser.lexer.lexeme(parser.previous);
    let number = lexeme.parse::<f64>().unwrap();
    compiler
        .emit_constant(Value::Number(number), parser.line_number())
        .inspect_err(|e| parser.error(e.clone()));
}

fn literal(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let operator_type = parser.previous.t;

    match operator_type {
        TokenType::Nil => compiler.emit_byte(OP_NIL, parser.line_number()),
        TokenType::True => compiler.emit_byte(OP_TRUE, parser.line_number()),
        TokenType::False => compiler.emit_byte(OP_FALSE, parser.line_number()),
        _ => unreachable!(),
    }
}

fn string(parser: &mut Parser, compiler: &mut Take<Compiler>, _assignable: bool) {
    let str_value = parser.lexer.string_lexeme(parser.previous);
    let heap_obj = Rc::new(RefCell::new(str_value.to_owned()));
    compiler
        .emit_constant(Value::StringRef(heap_obj), parser.line_number())
        .inspect_err(|e| parser.error(e.clone()));
}

fn variable(parser: &mut Parser, compiler: &mut Take<Compiler>, assignable: bool) {
    named_variable(parser, compiler, assignable);
}

fn named_variable(parser: &mut Parser, compiler: &mut Take<Compiler>, assignable: bool) {
    let (get_op, set_op, arg) = match resolve_local(parser, compiler) {
        Some(arg) => (OP_GET_LOCAL, OP_SET_LOCAL, arg),
        None => {
            let arg = identifier_constant(parser, compiler) as u8;
            (OP_GET_GLOBAL, OP_SET_GLOBAL, arg)
        }
    };

    if assignable && parser.r#match(TokenType::Equal) {
        expression(parser, compiler);
        compiler.emit_bytes(set_op, arg, parser.line_number());
    } else {
        compiler.emit_bytes(get_op, arg, parser.line_number());
    }
}

fn resolve_local(parser: &mut Parser, compiler: &mut Take<Compiler>) -> Option<u8> {
    for i in (0..compiler.local_count).rev() {
        let local = compiler.locals[i];
        if identifiers_equal(parser, local.name, parser.previous) {
            return Some(i as u8);
        }
    }

    None
}

struct ParseRule(
    Option<Box<dyn FnMut(&mut Parser, &mut Take<Compiler>, bool)>>,
    Option<Box<dyn FnMut(&mut Parser, &mut Take<Compiler>, bool)>>,
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

impl Take<Compiler> {
    fn push_compiler(&mut self, function_type: FunctionType, name: Option<&str>) {
        let enclosing = self.take();
        let _ = std::mem::replace(
            self,
            Take::new(Compiler::new(enclosing, function_type, name)),
        );
    }

    fn pop_compiler(&mut self) {
        let enclosing = *self.take().unwrap().enclosing.unwrap();
        let _ = std::mem::replace(self, Take::new(enclosing));
    }
}
