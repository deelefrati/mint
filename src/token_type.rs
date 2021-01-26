use crate::semantic_analyzer::Type;
impl std::fmt::Display for TokenType {
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
            //TokenType::BangEqual => write!(f, "!="),
            TokenType::BangEqualEqual => write!(f, "!=="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
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
            TokenType::Print => write!(f, "console.log"),
            TokenType::Return => write!(f, "return"),
            TokenType::This => write!(f, "this"),
            TokenType::Const => write!(f, "const"),
            TokenType::Eof => write!(f, "eof"),
            TokenType::Assert => write!(f, "assert"),
        }
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

    // One or two character tokens.
    Bang,
    // BangEqual,
    BangEqualEqual,
    Equal,
    EqualEqual,
    EqualEqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,

    // Literals.
    String(std::string::String),
    Identifier,
    Number(f64),

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
    Print,
    Return,
    This,
    Const,

    Eof,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VarType {
    Number,
    String,
    Boolean,
    Null,
    Function,
}
impl From<Type> for VarType {
    fn from(x: Type) -> Self {
        match x {
            Type::Num => VarType::Number,
            Type::Bool => VarType::Boolean,
            Type::Null => VarType::Null,
            Type::Str => VarType::String,
            Type::Fun(_, _, _, _) => VarType::Function,
        }
    }
}
