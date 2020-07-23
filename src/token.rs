use crate::token_type::TokenType;

pub struct Token {
    token_type: TokenType,
    line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize) -> Self {
        Self { token_type, line }
    }
}
