use crate::expr::Expr;
use crate::token_type::VarType;
#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    AssertStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
    Block(Vec<Stmt>),
}
