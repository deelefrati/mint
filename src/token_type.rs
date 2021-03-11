use crate::{semantic_analyzer::Type, token::Token};
use std::fmt::Display;
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Mod => write!(f, "%"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Comment => write!(f, "//"),
            TokenType::Star => write!(f, "*"),
            TokenType::Blank => write!(f, "blank"),
            TokenType::NewLine => write!(f, "new_line"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqualEqual => write!(f, "!=="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqualEqual => write!(f, "==="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::And => write!(f, "&&"),
            TokenType::Or => write!(f, "||"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::String(string) => write!(f, "{}", string),
            TokenType::Number(num) => write!(f, "{}", num),
            TokenType::Num => write!(f, "number"),
            TokenType::Bool => write!(f, "boolean"),
            TokenType::Str => write!(f, "str"),
            TokenType::Type => write!(f, "type"),
            TokenType::Else => write!(f, "else"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Function => write!(f, "function"),
            TokenType::If => write!(f, "if"),
            TokenType::Null => write!(f, "null"),
            TokenType::Log => write!(f, "log"),
            TokenType::Console => write!(f, "console"),
            TokenType::Return => write!(f, "return"),
            TokenType::This => write!(f, "this"),
            TokenType::Const => write!(f, "const"),
            TokenType::Eof => write!(f, "eof"),
            TokenType::Assert => write!(f, "assert"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::Typeof => write!(f, "typeof"),
            TokenType::Import => write!(f, "import"),
            TokenType::From => write!(f, "from"),
        }
    }
}

impl Default for TokenType {
    fn default() -> Self {
        TokenType::Blank
    }
}

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
    Mod,
    Semicolon,
    Colon,
    Slash,
    Comment,
    Star,
    Blank,
    NewLine,
    Pipe,

    // One or two character tokens.
    Bang,
    BangEqualEqual,
    Equal,
    EqualEqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,

    Number(f64),
    String(std::string::String),
    Identifier,

    // keywords
    Assert,
    Num,
    Str,
    Bool,
    Type,
    Else,
    True,
    False,
    Function,
    If,
    Null,
    Console,
    Log,
    Return,
    This,
    Const,
    Typeof,
    Import,
    From,

    Eof,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(num) => write!(f, "{}", num),
            Literal::String(string) => write!(f, "{}", string),
            Literal::Boolean(boolean) => write!(f, "{}", boolean),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(std::string::String),
    Boolean(bool),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Number(a), Literal::Number(b)) => a == b,
            (Literal::String(a), Literal::String(b)) => a == b,
            (Literal::Boolean(a), Literal::Boolean(b)) => a == b,
            _ => false,
        }
    }
}

impl Literal {
    pub fn to_primitive(&self) -> Type {
        match self {
            Literal::Number(_) => Type::Num,
            Literal::String(_) => Type::Str,
            Literal::Boolean(_) => Type::Bool,
        }
    }
}

impl std::fmt::Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Null => write!(f, "Null"),
            VarType::Number => write!(f, "Number"),
            VarType::Boolean => write!(f, "Boolean"),
            VarType::String => write!(f, "String"),
            VarType::Literals(literal) => write!(f, "{}", literal),
            VarType::Function | VarType::MintFun => write!(f, "Function"),
            VarType::UserType(mint_type) => write!(f, "{}", mint_type.lexeme()),
            VarType::Union(_) => write!(f, "Union"),
            VarType::Never => write!(f, "Never"),
            VarType::Object => write!(f, "Object"),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Number,
    String,
    Boolean,
    Null,
    Function,
    MintFun,
    Never,
    Literals(Literal),
    UserType(Token),
    Union(Vec<(VarType, Token)>),
    Object,
}
impl From<Type> for VarType {
    fn from(x: Type) -> Self {
        match x {
            Type::Num => VarType::Number,
            Type::Bool => VarType::Boolean,
            Type::Null => VarType::Null,
            Type::Str => VarType::String,
            Type::Literals(literal) => VarType::Literals(literal),
            Type::Fun(_, _, _, _) => VarType::Function,
            Type::UserType(mint_type) => VarType::UserType(mint_type.name),
            Type::Union(types) => VarType::Union(
                types
                    .iter()
                    .map(|(type_, token)| (type_.to_owned().into(), token.clone()))
                    .collect(),
            ),
            Type::Alias(t, _) => VarType::UserType(t),
            Type::Never => VarType::Never,
            Type::Object => VarType::Object,
            Type::MintFun(_, _, _) => VarType::MintFun,
        }
    }
}

impl VarType {
    pub fn has_user_type(&self) -> bool {
        match self {
            VarType::UserType(_) => true,
            VarType::Union(union) => union.iter().any(|(t, _)| matches!(t, VarType::UserType(_))),
            _ => false,
        }
    }

    pub fn get_tokens(&self) -> Vec<Token> {
        match self {
            VarType::UserType(token) => vec![token.clone()],
            VarType::Union(union) => union
                .iter()
                .filter(|(var_type, _)| matches!(var_type, VarType::UserType(_)))
                .map(|(_, token)| token.clone())
                .collect(),
            _ => vec![],
        }
    }
}
