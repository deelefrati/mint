use crate::expr::Expr;
use crate::token::Token;
use crate::token_type::VarType;
#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    AssertStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
    Block(Vec<Stmt>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    Function(
        Token,
        Vec<(Token, VarType)>,
        Vec<Stmt>,
        VarType,
        Option<Token>,
    ),
    Return(Token, Option<Expr>),
    TypeStmt(Token, Vec<(Token, VarType)>),
}
