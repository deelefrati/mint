use crate::expr::Value;

use super::{print_marker, static_error_template};

#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeError {
    DivisionByZero(usize, usize, usize),
    AssertionFailed(usize, usize, usize),
    VariableNotDeclared(usize, usize, usize),
    NotCallable(usize, usize, usize),
    ArityMismatch(usize, usize, usize, usize, usize),
    Return(Value),
}

impl RuntimeError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Runtime";
        match self {
            RuntimeError::DivisionByZero(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Error: Division by zero".to_string(),
                Some(print_marker(*starts_at, *ends_at, None)),
            ),
            RuntimeError::AssertionFailed(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Assertion Failed ".to_string(),
                Some(print_marker(*starts_at, *ends_at, None)),
            ),
            RuntimeError::VariableNotDeclared(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Variable not declared".to_string(),
                Some(print_marker(*starts_at, *ends_at, None)),
            ),
            RuntimeError::NotCallable(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Not callable".to_string(),
                Some(print_marker(*starts_at, *ends_at, None)),
            ),
            RuntimeError::ArityMismatch(line, starts_at, ends_at, expected, found) => {
                static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!(
                        "Wrong number of parameters. Expected {} and found {}",
                        expected, found
                    ),
                    Some(print_marker(*starts_at, *ends_at, None)),
                )
            }
            RuntimeError::Return(_) => "".to_string(),
        }
    }
}
