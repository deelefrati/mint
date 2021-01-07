use crate::expr::Expr;
use crate::token::Token;
use crate::token_type::VarType;
use std::rc::Rc;
#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    AssertStmt(Expr),
    VarStmt(String, Option<VarType>, Expr),
    Block(Vec<Stmt>),
    IfStmt(Expr, Vec<Stmt>, Vec<Stmt>),
    // Function(Token, Vec<Token>, Rc<Stmt>),
}
