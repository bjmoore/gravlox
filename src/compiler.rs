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
            TokenType::LeftParen    => ParseRule(Some(&grouping), None,          Precedence::None),
            TokenType::RightParen   => ParseRule(None,            None,          Precedence::None),
            TokenType::LeftBrace    => ParseRule(None,            None,          Precedence::None),
            TokenType::RightBrace   => ParseRule(None,            None,          Precedence::None),
            TokenType::Comma        => ParseRule(None,            None,          Precedence::None),
            TokenType::Dot          => ParseRule(None,            None,          Precedence::None),
            TokenType::Minus        => ParseRule(Some(&unary),    Some(&binary), Precedence::Term),
            TokenType::Plus         => ParseRule(None,            Some(&binary), Precedence::Term),
            TokenType::Semicolon    => ParseRule(None,            None,          Precedence::None),
            TokenType::Slash        => ParseRule(None,            Some(&binary), Precedence::Factor),
            TokenType::Star         => ParseRule(None,            Some(&binary), Precedence::Factor),
            TokenType::Bang         => ParseRule(None,            None,          Precedence::None),
            TokenType::BangEqual    => ParseRule(None,            None,          Precedence::None),
            TokenType::Equal        => ParseRule(None,            None,          Precedence::None),
            TokenType::EqualEqual   => ParseRule(None,            None,          Precedence::None),
            TokenType::Greater      => ParseRule(None,            None,          Precedence::None),
            TokenType::GreaterEqual => ParseRule(None,            None,          Precedence::None),
            TokenType::Less         => ParseRule(None,            None,          Precedence::None),
            TokenType::LessEqual    => ParseRule(None,            None,          Precedence::None),
            TokenType::Identifier   => ParseRule(None,            None,          Precedence::None),
            TokenType::String       => ParseRule(None,            None,          Precedence::None),
            TokenType::Number       => ParseRule(Some(&number),   None,          Precedence::None),
            TokenType::And          => ParseRule(None,            None,          Precedence::None),
            TokenType::Class        => ParseRule(None,            None,          Precedence::None),
            TokenType::Else         => ParseRule(None,            None,          Precedence::None),
            TokenType::False        => ParseRule(None,            None,          Precedence::None),
            TokenType::For          => ParseRule(None,            None,          Precedence::None),
            TokenType::Fun          => ParseRule(None,            None,          Precedence::None),
            TokenType::If           => ParseRule(None,            None,          Precedence::None),
            TokenType::Nil          => ParseRule(None,            None,          Precedence::None),
            TokenType::Or           => ParseRule(None,            None,          Precedence::None),
            TokenType::Print        => ParseRule(None,            None,          Precedence::None),
            TokenType::Return       => ParseRule(None,            None,          Precedence::None),
            TokenType::Super        => ParseRule(None,            None,          Precedence::None),
            TokenType::This         => ParseRule(None,            None,          Precedence::None),
            TokenType::True         => ParseRule(None,            None,          Precedence::None),
            TokenType::Var          => ParseRule(None,            None,          Precedence::None),
            TokenType::While        => ParseRule(None,            None,          Precedence::None),
            TokenType::Error        => ParseRule(None,            None,          Precedence::None),
            TokenType::Eof          => ParseRule(None,            None,          Precedence::None),
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
