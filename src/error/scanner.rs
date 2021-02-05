use super::{print_marker, static_error_template};
use crate::token_type::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub enum ScannerError {
    InvalidToken(usize, TokenType, usize, usize),
    InvalidNumber(usize, TokenType, usize, usize),
    InvalidString(usize, usize, usize),
    UnterminatedString(usize),
}

impl ScannerError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Syntax";
        match self {
            ScannerError::InvalidToken(line,tt, line_start, line_end) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*line_start),
                Some(*line_end),
                format!("Invalid token. Did you mean {} ?", tt),
                Some(print_marker(*line_start, *line_end, Some("here"))),
            ),
            ScannerError::UnterminatedString(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
               "Unterminated String. Note: Every string must start and finish with a quotation mark, (e.g \"string\"). Maybe you forgot one?".to_string(),
                None,
            ),
            ScannerError::InvalidNumber(line,number, line_start, line_end) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*line_start),
                Some(*line_end),
                format!("Failed parsing number {}", number),
                Some(print_marker(*line_start, *line_end, Some("Here"))),
            ),
            ScannerError::InvalidString(line, line_start, line_end) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*line_start),
                Some(*line_end),
                "Invalid character".to_string(),
                Some(print_marker(*line_start, *line_end, Some("Here"))),
            ),
        }
    }
}
