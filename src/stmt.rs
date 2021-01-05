use crate::expr::Expr;
use crate::token_type::VarType;
#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    AssertStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
    Block(Vec<Stmt>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
}
