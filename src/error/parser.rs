use crate::token_type::TokenType;

use super::static_error_template;

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    Missing(usize, TokenType),
    MissingExpression(usize),
    // declaration of variables errors
    AssignmentExpected(usize),
    TypeNotDefined(usize),
    ColonExpected(usize),
    SemicolonExpected(usize),
    IdentifierExpected(usize),
    Expected(usize, TokenType),
    ReturnTypeExpected(usize),
    EmptyType, // TODO completar o erro
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
            ParserError::AssignmentExpected(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected \"=\"".to_string(),
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
            ParserError::ColonExpected(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected \":\"".to_string(),
                None,
            ),
            ParserError::SemicolonExpected(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected \";\"".to_string(),
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
                format!("Expected {:?}", tt),
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
        }
    }
}
