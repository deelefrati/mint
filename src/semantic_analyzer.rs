use crate::{expr::*, stmt::Stmt};
use std::collections::HashMap;
#[derive(Debug)]
pub enum Type {
    Num,
    Bool,
    Null,
    Str,
}
#[derive(Default)]
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn analyze(&mut self, stmts: &'a Vec<Stmt>) -> Result<(), ()> {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) => self.insert(&expr, t),
                    Err(()) => return Err(()),
                },
            }
        }
        println!("{:?}", self.types);
        Ok(())
    }

    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn analyze_one(&self, expr: &Expr) -> Result<Type, ()> {
        match expr {
            Expr::Arithmetic(a, (op, _), b) => self.analyze_arith(a, op, b),
            Expr::Comparation(a, (op, _), b) => self.analyze_comp(a, op, b),
            Expr::Logical(a, (op, _), b) => self.analyze_logical(a, op, b),
            Expr::Unary((op, _), a) => self.analyze_unary(op, a),
            Expr::Grouping(expr) => self.analyze_one(expr),
            Expr::Literal((value, _)) => Ok(self.analyze_literal(value)),
        }
    }

    fn analyze_unary(&self, op: &UnaryOp, exp: &Expr) -> Result<Type, ()> {
        use UnaryOp::*;
        let exp_type = self.analyze_one(exp)?;

        let t = match (op, exp_type) {
            (Bang, Type::Bool) => Type::Bool,
            (Minus, Type::Num) => Type::Num,
            (Plus, Type::Num) => Type::Num,
            (_, _) => return Err(()),
        };
        Ok(t)
    }

    fn analyze_arith(&self, a: &Expr, op: &ArithmeticOp, b: &Expr) -> Result<Type, ()> {
        use ArithmeticOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (Add, Type::Num, Type::Num) => Type::Num,
            (Add, Type::Str, Type::Str) => Type::Str,

            (Sub, Type::Num, Type::Num) => Type::Num,

            (Mul, Type::Num, Type::Num) => Type::Num,

            (Div, Type::Num, Type::Num) => Type::Num,

            (Mod, Type::Num, Type::Num) => Type::Num,

            (_, _, _) => return Err(()),
        };

        Ok(expr_type)
    }

    fn analyze_logical(&self, a: &Expr, op: &LogicalOp, b: &Expr) -> Result<Type, ()> {
        use LogicalOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Bool, Type::Bool) => Type::Bool,

            (Or, Type::Bool, Type::Bool) => Type::Bool,

            (_, _, _) => return Err(()),
        };

        Ok(expr_type)
    }

    fn analyze_comp(&self, a: &Expr, op: &ComparationOp, b: &Expr) -> Result<Type, ()> {
        use ComparationOp::*;
        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;
        let expr_type = match (op, type_a, type_b) {
            (Equal, _, _) => Type::Bool,

            (NotEqual, _, _) => Type::Bool,

            (StrictNotEqual, Type::Num, Type::Num) => Type::Bool,
            (StrictNotEqual, Type::Bool, Type::Bool) => Type::Bool,
            (StrictNotEqual, Type::Str, Type::Str) => Type::Bool,
            (StrictNotEqual, _, Type::Null) => Type::Bool,
            (StrictNotEqual, Type::Null, _) => Type::Bool,

            (StrictEqual, Type::Num, Type::Num) => Type::Bool,
            (StrictEqual, Type::Bool, Type::Bool) => Type::Bool,
            (StrictEqual, Type::Str, Type::Str) => Type::Bool,
            (StrictEqual, _, Type::Null) => Type::Bool,
            (StrictEqual, Type::Null, _) => Type::Bool,

            (LessThan, Type::Num, Type::Num) => Type::Bool,
            (LessThan, Type::Str, Type::Str) => Type::Bool,

            (LessEqual, Type::Num, Type::Num) => Type::Bool,
            (LessEqual, Type::Str, Type::Str) => Type::Bool,

            (GreaterThan, Type::Num, Type::Num) => Type::Bool,
            (GreaterThan, Type::Str, Type::Str) => Type::Bool,

            (GreaterEqual, Type::Num, Type::Num) => Type::Bool,
            (GreaterEqual, Type::Str, Type::Str) => Type::Bool,

            (_, _, _) => return Err(()),
        };
        Ok(expr_type)
    }

    fn analyze_literal(&self, value: &Value) -> Type {
        match value {
            Value::Null => Type::Null,
            Value::Boolean(_) => Type::Bool,
            Value::Number(_) => Type::Num,
            Value::Str(_) => Type::Str,
        }
    }
}
