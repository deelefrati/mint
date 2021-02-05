use crate::token_type::TokenType;

use super::static_error_template;

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    Missing(usize, TokenType),
    MissingExpression(usize),
    TypeNotDefined(usize),
    IdentifierExpected(usize),
    Expected(usize, Vec<TokenType>),
    ReturnTypeExpected(usize),
    EmptyType, // TODO completar o erro
    TypeExpected(usize),
}

impl ParserError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Syntax";

        match self {
            ParserError::Missing(line, tt) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                format!("Missing: {:?}", tt),
                None,
            ),
            ParserError::MissingExpression(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Missing an expression".to_string(),
                None,
            ),
            ParserError::TypeNotDefined(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected type after \":\"".to_string(),
                None,
            ),
            ParserError::IdentifierExpected(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Name of the variable must be provided afeter the \"const\" keyword.".to_string(),
                None,
            ),
            ParserError::Expected(line, tt) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                if tt.len() > 1 {
                    format!("Expected {:?}", tt)
                } else {
                    format!("Expected one os this symbols {:?}", tt)
                },
                None,
            ),
            ParserError::ReturnTypeExpected(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected a return type".to_string(),
                None,
            ),
            ParserError::EmptyType => "empty type".to_string(),
            ParserError::TypeExpected(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                format!(
                    "Expect one of this types {} {} {} {} {} ",
                    TokenType::Num,
                    TokenType::Str,
                    TokenType::Bool,
                    TokenType::Null,
                    TokenType::Identifier
                ),
                None,
            ),
        }
    }
}
