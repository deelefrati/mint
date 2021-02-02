#![allow(dead_code)]
use crate::token_type::TokenType;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Token {
    token_type: TokenType,
    starts_at: usize,
    ends_at: usize,
    line: usize,
    lexeme: String,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        line: usize,
        starts_at: usize,
        ends_at: usize,
        lexeme: String,
    ) -> Self {
        Self {
            token_type,
            line,
            starts_at,
            ends_at,
            lexeme,
        }
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn starts_at(&self) -> usize {
        self.starts_at
    }

    pub fn ends_at(&self) -> usize {
        self.ends_at
    }

    pub fn line(&self) -> usize {
        self.line
    }
    pub fn lexeme(&self) -> String {
        self.lexeme.clone()
    }
}
