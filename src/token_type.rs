#![allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
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
    Comment,
    Star,
    Blank,
    NewLine,

    // One or two character tokens.
    Bang,
    BangEqual,
    BangEqualEqual,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,

    // Literals.
    Identifier(std::string::String),
    String(std::string::String),
    Number(f64),

    // Keywords.
    Class,
    Else,
    True,
    False,
    Function,
    If,
    Null,
    Print,
    Return,
    This,
    Const,

    Eof,
}
