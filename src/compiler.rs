use crate::token::Token;
use crate::token::TokenType;

pub fn compile(source: &str) {}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
}
struct ParseRule(
    Option<&'static dyn FnMut(&mut Parser)>,
    Option<&'static dyn FnMut(&mut Parser)>,
    Precedence,
);

fn expression(compiler: &Parser) {}

fn unary(compiler: &mut Parser) {
    let operator_type: TokenType = compiler.previous.t;

    expression(&compiler);
}

fn binary(compiler: &mut Parser) {}

fn grouping(compiler: &mut Parser) {}

fn number(compiler: &mut Parser) {}

impl<'a> Parser<'a> {
    fn getRule(&self, t: TokenType) {
        #[rustfmt::skip]
        match t {
            TokenType::LeftParen    => ParseRule(Some(&grouping), None,          Precedence::Term),
            TokenType::RightParen   => ParseRule(None,            None,          Precedence::Term),
            TokenType::LeftBrace    => ParseRule(None,            None,          Precedence::Term),
            TokenType::RightBrace   => ParseRule(None,            None,          Precedence::Term),
            TokenType::Comma        => ParseRule(None,            None,          Precedence::Term),
            TokenType::Dot          => ParseRule(None,            None,          Precedence::Term),
            TokenType::Minus        => ParseRule(Some(&unary),    Some(&binary), Precedence::Term),
            TokenType::Plus         => ParseRule(None,            Some(&binary), Precedence::Term),
            TokenType::Semicolon    => ParseRule(None,            None,          Precedence::Term),
            TokenType::Slash        => ParseRule(None,            Some(&binary), Precedence::Term),
            TokenType::Star         => ParseRule(None,            Some(&binary), Precedence::Term),
            TokenType::Bang         => ParseRule(None,            None,          Precedence::Term),
            TokenType::BangEqual    => ParseRule(None,            None,          Precedence::Term),
            TokenType::Equal        => ParseRule(None,            None,          Precedence::Term),
            TokenType::EqualEqual   => ParseRule(None,            None,          Precedence::Term),
            TokenType::Greater      => ParseRule(None,            None,          Precedence::Term),
            TokenType::GreaterEqual => ParseRule(None,            None,          Precedence::Term),
            TokenType::Less         => ParseRule(None,            None,          Precedence::Term),
            TokenType::LessEqual    => ParseRule(None,            None,          Precedence::Term),
            TokenType::Identifier   => ParseRule(None,            None,          Precedence::Term),
            TokenType::String       => ParseRule(None,            None,          Precedence::Term),
            TokenType::Number       => ParseRule(Some(&number),   None,          Precedence::Term),
            TokenType::And          => ParseRule(None,            None,          Precedence::Term),
            TokenType::Class        => ParseRule(None,            None,          Precedence::Term),
            TokenType::Else         => ParseRule(None,            None,          Precedence::Term),
            TokenType::False        => ParseRule(None,            None,          Precedence::Term),
            TokenType::For          => ParseRule(None,            None,          Precedence::Term),
            TokenType::Fun          => ParseRule(None,            None,          Precedence::Term),
            TokenType::If           => ParseRule(None,            None,          Precedence::Term),
            TokenType::Nil          => ParseRule(None,            None,          Precedence::Term),
            TokenType::Or           => ParseRule(None,            None,          Precedence::Term),
            TokenType::Print        => ParseRule(None,            None,          Precedence::Term),
            TokenType::Return       => ParseRule(None,            None,          Precedence::Term),
            TokenType::Super        => ParseRule(None,            None,          Precedence::Term),
            TokenType::This         => ParseRule(None,            None,          Precedence::Term),
            TokenType::True         => ParseRule(None,            None,          Precedence::Term),
            TokenType::Var          => ParseRule(None,            None,          Precedence::Term),
            TokenType::While        => ParseRule(None,            None,          Precedence::Term),
            TokenType::Error        => ParseRule(None,            None,          Precedence::Term),
            TokenType::Eof          => ParseRule(None,            None,          Precedence::Term),
        };
    }
}

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
