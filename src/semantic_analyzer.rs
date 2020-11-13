use crate::{
    error::{Error, SemanticError},
    expr::*,
    stmt::Stmt,
    token_type::VarType,
};
use std::collections::HashMap;

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => write!(f, "Null"),
            Type::Num => write!(f, "Number"),
            Type::Bool => write!(f, "Boolean"),
            Type::Str => write!(f, "String"),
        }
    }
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Num,
    Bool,
    Null,
    Str,
}
#[derive(Default)]
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
    symbol_table: HashMap<&'a str, Type>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn analyze(&mut self, stmts: &'a [Stmt]) -> Result<(), Vec<Error>> {
        let mut errors: Vec<Error> = vec![];
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) => self.insert(&expr, t),
                    Err(semantic_error) => errors.push(Error::Semantic(semantic_error)),
                },
                Stmt::AssertStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) if t != Type::Bool => errors.push(Error::Semantic(
                        SemanticError::MismatchedTypes(Type::Bool, t, None),
                    )),
                    Err(error) => errors.push(Error::Semantic(error)),
                    _ => {}
                },
                Stmt::VarStmt(var_name, var_type, expr) => match self.analyze_one(&expr) {
                    Ok(t) => match (var_type, t) {
                        (Some(VarType::Boolean), Type::Bool) => {
                            if let Err(error) = self.insert_var(var_name, Type::Bool) {
                                errors.push(error);
                            }
                        }
                        (Some(VarType::String), Type::Str) => {
                            if let Err(error) = self.insert_var(var_name, Type::Str) {
                                errors.push(error);
                            }
                        }
                        (Some(VarType::Number), Type::Num) => {
                            if let Err(error) = self.insert_var(var_name, Type::Num) {
                                errors.push(error);
                            }
                        }
                        (Some(VarType::Null), Type::Null) => {
                            if let Err(error) = self.insert_var(var_name, Type::Null) {
                                errors.push(error);
                            }
                        }
                        (None, _) => {
                            if let Err(error) = self.insert_var(var_name, t) {
                                errors.push(error);
                            }
                        }
                        _ => errors.push(Error::Semantic(SemanticError::IncompatibleDeclaration)),
                    },
                    Err(semantic_error) => errors.push(Error::Semantic(semantic_error)),
                },
                Stmt::Block(_) => {}
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn insert_var(&mut self, identifier: &'a str, t: Type) -> Result<(), Error> {
        if !self.symbol_table.contains_key(identifier) {
            self.symbol_table.insert(identifier, t);
            Ok(())
        } else {
            Err(Error::Semantic(SemanticError::VariableOverwrited))
        }
    }

    fn get_var(&mut self, identifier: &str) -> Option<&Type> {
        self.symbol_table.get(identifier)
    }

    fn analyze_one(&mut self, expr: &Expr) -> Result<Type, SemanticError> {
        match expr {
            Expr::Arithmetic(a, (op, _), b) => self.analyze_arith(a, op, b),
            Expr::Comparation(a, (op, _), b) => self.analyze_comp(a, op, b),
            Expr::Logical(a, (op, _), b) => self.analyze_logical(a, op, b),
            Expr::Unary((op, _), a) => self.analyze_unary(op, a),
            Expr::Grouping(expr) => self.analyze_one(expr),
            Expr::Literal((value, _)) => Ok(self.analyze_literal(value)),
            Expr::Variable(_, identifier) => self.analyze_var_expr(identifier),
        }
    }

    fn analyze_var_expr(&mut self, identifier: &str) -> Result<Type, SemanticError> {
        if let Some(t) = self.get_var(identifier) {
            Ok(*t)
        } else {
            Err(SemanticError::VariableNotDeclared)
        }
    }

    fn analyze_unary(&mut self, op: &UnaryOp, exp: &Expr) -> Result<Type, SemanticError> {
        use UnaryOp::*;
        let exp_type = self.analyze_one(exp)?;

        let t = match (op, exp_type) {
            (Bang, Type::Bool) => Type::Bool,
            (Minus, Type::Num) => Type::Num,
            (Plus, Type::Num) => Type::Num,
            (op, t) => return Err(SemanticError::IncompatibleUnaryOp(*op, t)),
        };
        Ok(t)
    }

    fn analyze_arith(
        &mut self,
        a: &Expr,
        op: &ArithmeticOp,
        b: &Expr,
    ) -> Result<Type, SemanticError> {
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

            (op, left, right) => return Err(SemanticError::IncompatibleArith(*op, left, right)),
        };

        Ok(expr_type)
    }

    fn analyze_logical(
        &mut self,
        a: &Expr,
        op: &LogicalOp,
        b: &Expr,
    ) -> Result<Type, SemanticError> {
        use LogicalOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Bool, Type::Bool) => Type::Bool,
            (Or, Type::Bool, Type::Bool) => Type::Bool,

            (op, left, right) => return Err(SemanticError::IncompatibleLogicOp(*op, left, right)),
        };

        Ok(expr_type)
    }

    fn analyze_comp(
        &mut self,
        a: &Expr,
        op: &ComparationOp,
        b: &Expr,
    ) -> Result<Type, SemanticError> {
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

            (op, left, right) => {
                return Err(SemanticError::IncompatibleComparation(
                    *op, left, right, None,
                ))
            }
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
