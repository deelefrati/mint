use crate::expr::{ArithmeticOp, ComparationOp, LogicalOp, UnaryOp};
use crate::semantic_analyzer::Type;
use crate::token_type::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Input(String),
    Scanner(ScannerError),
    Parser(ParserError),
    Semantic(SemanticError),
    Runtime(RuntimeError),
    Unexpected,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeError {
    GenericError,
    DivisionByZero(usize, usize, usize),
    AssertionFailed,
    VariableNotDeclared,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScannerError {
    InvalidToken(usize, String, usize, usize),
    UnterminatedString(usize),
}

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
}

#[derive(Debug, PartialEq, Clone)]
pub enum SemanticError {
    MismatchedTypes(Type, Type, Option<String>),
    IncompatibleArith(ArithmeticOp, Type, Type),
    IncompatibleComparation(ComparationOp, Type, Type, Option<String>),
    IncompatibleLogicOp(LogicalOp, Type, Type),
    IncompatibleUnaryOp(UnaryOp, Type),
    IncompatibleDeclaration,
    VariableNotDeclared,
    VariableOverwrited,
}

#[allow(dead_code)]
enum Color {
    Reset,
    White,
    Red,
    Green,
    Blue,
    Yellow,
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Color::*;
        match self {
            Reset => write!(f, "\x1b[0m"),
            White => write!(f, "\x1b[1;37m"),
            Red => write!(f, "\x1b[1;31m"),
            Green => write!(f, "\x1b[1;32m"),
            Blue => write!(f, "\x1b[1;34m"),
            Yellow => write!(f, "\x1b[1;33m"),
        }
    }
}

impl Error {
    fn blue_pipe(&self) -> String {
        format!("{}|{}", Color::Blue, Color::Reset)
    }

    pub fn show_error(&self, file: Option<&str>, source_vec: Option<&[String]>) {
        if let Some(filename) = file {
            eprintln!(
                "{}Error in file {}'{}':",
                Color::Red,
                Color::Reset,
                filename
            )
        } else {
            eprintln!("{}Error{}:", Color::Red, Color::Reset)
        }
        eprintln!("{} {}", self.blue_pipe(), self.format_error(source_vec));
    }

    fn format_error(&self, source_vec: Option<&[String]>) -> String {
        use Error::*;

        match self {
            Input(reason) => format!(
                "{}Input error: {}\n{}",
                Color::White,
                reason,
                self.blue_pipe(),
            ),
            Unexpected => format!(
                "{}Unexpected fail: Something went wrong while parsing the file{}",
                Color::White,
                Color::Reset
            ),
            Scanner(scanner_error) => self.format_scanner_error(scanner_error, source_vec.unwrap()),
            Parser(parser_error) => self.format_parser_error(parser_error, source_vec.unwrap()),
            Semantic(semantic_error) => {
                self.format_semantic_error(semantic_error, source_vec.unwrap())
            }
            Runtime(_) => "RuntimeError".into(),
        }
    }

    fn format_scanner_error(&self, error: &ScannerError, source_vec: &[String]) -> String {
        use ScannerError::*;
        match error {
            InvalidToken(line,note, line_start, line_end) => format!(
                "{}Syntax error in line {} from column {} to {}: \n{}\n{} '{}'\n{}{}\n{} {}Reason: {}{}",
                Color::White,
                line,
                line_start,
                line_end,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line -1).unwrap(),
                self.blue_pipe(),
                self.print_marker(*line_start, *line_end),
                self.blue_pipe(),
                Color::Yellow,
                note,
                Color::Reset
            ),
            UnterminatedString(line) => format!(
                "{}Unterminated String error from line {} to the end of file:\n{} \n{}'{}'\n{}\n{} {}Note: Every string must start and finish with a quotation mark, (e.g \"string\"). Maybe you forgot one?{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line -1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
        }
    }

    fn format_parser_error(&self, error: &ParserError, source_vec: &[String]) -> String {
        use ParserError::*;
        match error {
            Missing(line, string) => format!(
                "{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}{:?}{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                string,
                Color::Reset,
            ),
            MissingExpression(line) => format!(
                "{}Syntax error in line {}:\n{}\n{} '{}'\n{}\n{}{} Reason: Missing an expression{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
            AssignmentExpected(line) => format!(
                "{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}{}{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                "Expected \"=\"",
                Color::Reset,
            ),
            TypeNotDefined(line) => format!(
                "{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}{}{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                "Expected type after \":\"",
                Color::Reset,
            ),
            ColonExpected(line) => format!(
                "{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}{}{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                "Expected \":\"",
                Color::Reset,
            ),
            SemicolonExpected(line) => format!(
                "{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}{}{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                "Expected \";\"",
                Color::Reset,
            ),
            IdentifierExpected(line) => format!(
                "{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}{}{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                "Name of the variable must be provided after the \"const\" keyword.",
                Color::Reset,
            ),
        }
    }

    fn format_semantic_error(&self, error: &SemanticError, _source_vec: &[String]) -> String {
        match error {
            SemanticError::MismatchedTypes(expected, found, note) => {
                if note.is_some() {
                    format!(
                        "{}Mismatched types error: Expected {}{}{}, found {}{}{}\n{}\n{} {}",
                        Color::White,
                        Color::Yellow,
                        expected,
                        Color::White,
                        Color::Yellow,
                        found,
                        Color::White,
                        self.blue_pipe(),
                        self.blue_pipe(),
                        note.as_ref().unwrap()
                    )
                } else {
                    format!(
                        "{}Mismatched types error: Expected {}{}{}, found {}{}{}",
                        Color::White,
                        Color::Yellow,
                        expected,
                        Color::White,
                        Color::Yellow,
                        found,
                        Color::White,
                    )
                }
            }
            SemanticError::IncompatibleArith(op, left, right) => {
                format!(
                    "{}Incompatible operation error: Cannot use the {}'{}'{} binary operator with {}{}{} and {}{}{}.",
                    Color::White,
                    Color::Yellow,
                    op,
                    Color::White,
                    Color::Yellow,
                    left,
                    Color::White,
                    Color::Yellow,
                    right,
                    Color::Reset
                )
            },
            SemanticError::IncompatibleUnaryOp(op, t) => format!("{} The unary '{}' operator expects the following expression to be of type {}{}{}, but the expression evaluates to {}{}{}.", Color::White, op, Color::Yellow, Type::Num, Color::White, Color::Yellow, t, Color::Reset),
            SemanticError::IncompatibleComparation(op, l, r, note) => {
                if note.is_some() {
                    format!("{}Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}\n{}\n{} {}", Color::White, Color::Yellow, op, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::Reset, self.blue_pipe(), self.blue_pipe(), note.as_ref().unwrap())
                } else {
                    format!("{}Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}", Color::White, Color::Yellow, op, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::Reset)

                }
            }
            SemanticError::IncompatibleLogicOp(op, l, r) => format!("{}The {}'{}'{} operator expects the left and right expressions to be both of type {}{}{} or {}{}{}, but the expressions evaluates to {}{}{} and {}{}{} respectively.", Color::White, Color::Yellow, op, Color::White, Color::Yellow, Type::Bool, Color::White, Color::Yellow, Type::Null, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::White),
            SemanticError::IncompatibleDeclaration => "The type is not compatible with de assignment".to_string(),
            SemanticError::VariableNotDeclared => "Variable not declared".to_string(), // TODO melhorar msg erro
            SemanticError::VariableOverwrited => "Variable overwrited".to_string() // TODO melhorar msg erro
        }
    }

    fn print_marker(&self, start: usize, end: usize) -> String {
        let mut arrow: String = "  ".to_string();
        for i in 0..end {
            if i >= start {
                arrow.push('^');
            } else {
                arrow.push(' ');
            }
        }
        arrow
    }
}
