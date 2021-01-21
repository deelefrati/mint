use crate::{
    expr::{ArithmeticOp, ComparationOp, LogicalOp, UnaryOp},
    semantic_analyzer::Type,
    token_type::VarType,
};

use super::{print_marker, static_error_template};
#[derive(Debug, PartialEq, Clone)]
pub enum SemanticError {
    MismatchedTypes(usize, usize, usize, Type, Type),
    IncompatibleArith(usize, usize, usize, ArithmeticOp, Type, Type),
    IncompatibleComparation(usize, usize, usize, ComparationOp, Type, Type),
    IncompatibleLogicOp(usize, usize, usize, LogicalOp, Type, Type),
    IncompatibleUnaryOp(usize, usize, usize, UnaryOp, Type),
    IncompatibleDeclaration(usize, usize, usize, VarType, Type),
    VariableNotDeclared(usize, usize, usize, String),
    VariableOverwrited(usize, usize, usize, String),
    FunctionNotDeclared(usize, usize, usize),
    ArityMismatch(usize, usize, usize, usize, usize),
    ReturnTopLever(usize, usize, usize),
    MissingReturnStmt(usize, usize, usize),
}

impl SemanticError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Semantic";

        match self {
            SemanticError::MismatchedTypes(line, starts_at, ends_at, expected, found) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Expected {} found {}", expected, found),
                Some(print_marker(*starts_at, *ends_at, None)),
            ),
            SemanticError::IncompatibleArith(line, starts_at, ends_at,op, left, right) => static_error_template(
                    error_type,
                    source_vec,
                   *line,
                   Some(*starts_at),
                   Some(*ends_at),
                   format!("Incompatible operation error: Cannot use the {} operator with {} and {}", op, left, right),
                   Some(print_marker(*starts_at, *ends_at, None)),
                ),
            SemanticError::IncompatibleUnaryOp(line, starts_at, ends_at, op, t) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The unary operator expects the following expression to be of type {:?}, but the expression evaluates to {:?}", op, t),
                Some(print_marker(*starts_at, *ends_at, None))),
            SemanticError::IncompatibleComparation(line, starts_at, ends_at, op, l, r) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!("Incompatible comparation: Cannot compare using the {} operator with {} and {}", op, l, r),
                    Some(print_marker(*starts_at, *ends_at, None)),
                    ),
            SemanticError::IncompatibleLogicOp(line, starts_at, ends_at,op, l, r) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!("The {} operator expects the left and right expression to be both of type {} or {}, but the expression evaluates to {} and {} respectively.", op, Type::Bool, Type::Null, l, r),
                    Some(print_marker(*starts_at, *ends_at, None)),
                ),
            SemanticError::IncompatibleDeclaration(line, starts_at, ends_at, expected, found) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!("Declared type is {:?}, but the assigned expression evaluates to {}", expected, found),
                    Some(print_marker(*starts_at, *ends_at, Some(&format!("evaluates to {}", found)))),
                ),
            SemanticError::VariableNotDeclared(line, starts_at, ends_at, var_name) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!("Attempting to read an undeclared variable '{}'", var_name),
                    Some(print_marker(*starts_at, *ends_at, None)),
                ),
            SemanticError::VariableOverwrited(line, starts_at, ends_at, var_name) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!("Attempting to reassign the variable '{}'", var_name),
                    Some(print_marker(*starts_at, *ends_at, None)),
                ),
            SemanticError::FunctionNotDeclared(line, starts_at, ends_at) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    "Function not declared".to_string(),
                    Some(print_marker(*starts_at, *ends_at, None)),
                ),
            SemanticError::ArityMismatch(line, starts_at, ends_at, expected, found) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!("Wrong number of parameters. Expected {} and found {}", expected, found),
                    Some(print_marker(*starts_at, *ends_at, None)),
                ),
                SemanticError::ReturnTopLever(line, starts_at, ends_at) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    "Return statement can only be called inside a function body".to_string(),
                    Some(print_marker(*starts_at, *ends_at, None)),
                    ),
                SemanticError::MissingReturnStmt(line, starts_at, ends_at) => static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    "Function lacks ending return statement and return type does not include 'Null'".to_string(),
                    Some(print_marker(*starts_at, *ends_at, None)),
                    ),
        }
    }
}
