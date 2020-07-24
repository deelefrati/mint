#![allow(dead_code)]
use crate::token_type::TokenType;

pub struct Token {
    token_type: TokenType,
    starts_at: usize,
    ends_at: usize,
    line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, starts_at: usize, ends_at: usize) -> Self {
        Self {
            token_type,
            line,
            starts_at,
            ends_at,
        }
    }
}
