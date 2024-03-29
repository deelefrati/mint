use crate::expr::Expr;
use crate::token::Token;
use crate::token_type::VarType;
use std::collections::HashMap;
#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    AssertStmt(Expr),
    PrintStmt(Vec<Expr>),
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
    TypeStmt(Token, HashMap<String, VarType>),
    TypeAlias(Token, (VarType, Token)),
    ImportStmt(Token, Vec<Token>),
}
