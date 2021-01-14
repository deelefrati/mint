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
            Type::Fun(_, _) => write!(f, "Function"),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Num,
    Bool,
    Null,
    Str,
    Fun(Vec<VarType>, VarType),
}
impl std::convert::From<&VarType> for Type {
    fn from(var_type: &VarType) -> Self {
        match var_type {
            VarType::Number => Type::Num,
            VarType::Boolean => Type::Bool,
            VarType::Null => Type::Null,
            VarType::String => Type::Str,
            VarType::Function => Type::Fun(Vec::default(), VarType::Null),
        }
    }
}
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
    symbol_table: Vec<HashMap<String, Type>>,
    errors: Vec<Error>,
}

impl<'a> Default for SemanticAnalyzer<'a> {
    fn default() -> Self {
        SemanticAnalyzer::new()
    }
}
impl<'a> SemanticAnalyzer<'a> {
    pub fn new() -> Self {
        SemanticAnalyzer {
            types: HashMap::default(),
            symbol_table: vec![HashMap::default()],
            errors: vec![],
        }
    }

    pub fn analyze(&mut self, stmts: &'a [Stmt]) -> Result<(), Vec<Error>> {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) => self.insert(&expr, t),
                    Err(semantic_error) => self.errors.push(Error::Semantic(semantic_error)),
                },
                Stmt::AssertStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) if t != Type::Bool => {
                        self.errors
                            .push(Error::Semantic(SemanticError::MismatchedTypes(
                                Type::Bool,
                                t,
                                None,
                            )))
                    }
                    Err(error) => self.errors.push(Error::Semantic(error)),
                    _ => {}
                },
                Stmt::VarStmt(var_name, var_type, expr) => match self.analyze_one(&expr) {
                    Ok(t) => match (var_type, t.clone()) {
                        // TODO esse clone ta certo?
                        (Some(VarType::Boolean), Type::Bool) => {
                            if let Err(error) = self.insert_var(var_name, Type::Bool) {
                                self.errors.push(error);
                            }
                        }
                        (Some(VarType::String), Type::Str) => {
                            if let Err(error) = self.insert_var(var_name, Type::Str) {
                                self.errors.push(error);
                            }
                        }
                        (Some(VarType::Number), Type::Num) => {
                            if let Err(error) = self.insert_var(var_name, Type::Num) {
                                self.errors.push(error);
                            }
                        }
                        (Some(VarType::Null), Type::Null) => {
                            if let Err(error) = self.insert_var(var_name, Type::Null) {
                                self.errors.push(error);
                            }
                        }
                        (None, _) => {
                            if let Err(error) = self.insert_var(var_name, t) {
                                self.errors.push(error);
                            }
                        }
                        _ => self
                            .errors
                            .push(Error::Semantic(SemanticError::IncompatibleDeclaration)),
                    },
                    Err(semantic_error) => self.errors.push(Error::Semantic(semantic_error)),
                },
                Stmt::Block(stmts) => self.with_new_env(|analyzer| {
                    if let Err(nested_errors) = analyzer.analyze(stmts) {
                        for err in nested_errors {
                            analyzer.errors.push(err);
                        }
                    }
                }),
                Stmt::IfStmt(cond, then, else_) => {
                    match self.analyze_one(&cond) {
                        Ok(t) => self.insert(&cond, t),
                        Err(semantic_error) => self.errors.push(Error::Semantic(semantic_error)),
                    }
                    self.with_new_env(|analyzer| {
                        if let Err(nested_errors) = analyzer.analyze(then) {
                            for err in nested_errors {
                                analyzer.errors.push(err);
                            }
                        }
                    });
                    self.with_new_env(|analyzer| {
                        if let Err(nested_errors) = analyzer.analyze(else_) {
                            for err in nested_errors {
                                analyzer.errors.push(err);
                            }
                        }
                    });
                }
                Stmt::Function(token, params, body, return_type) => {
                    let mut param_types = vec![];
                    for (_, param_type) in params {
                        param_types.push(*param_type);
                    }
                    if let Err(err) =
                        self.insert_var(&token.lexeme(), Type::Fun(param_types, *return_type))
                    {
                        self.errors.push(err)
                    }
                    self.with_new_env(|analyzer| {
                        for (param, type_) in params {
                            if let Err(err) = analyzer.insert_var(
                                // TODO arrumar o into
                                &param.lexeme(),
                                type_.into(),
                            ) {
                                analyzer.errors.push(err);
                            }
                        }
                        if let Err(nested_errors) = analyzer.analyze(body) {
                            for err in nested_errors {
                                analyzer.errors.push(err);
                            }
                        }
                    })
                }
                Stmt::Return(_expr) => {
                    //println!("{:?}", expr);
                }
            }
        }
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn insert_var(&mut self, id: &str, t: Type) -> Result<(), Error> {
        let last_env = self.symbol_table.last_mut().unwrap();
        if last_env.contains_key(id) {
            Err(Error::Semantic(SemanticError::VariableOverwrited))
        } else {
            last_env.insert(id.to_string(), t);
            Ok(())
        }
    }
    //fn get_var(&mut self, id: &str) -> Result<&Type, SemanticError> {
    //    let last_env = self.symbol_table.last_mut().unwrap();
    //    if let Some(fun) = last_env.get(id) {
    //        Ok(fun)
    //    } else {
    //        Err(SemanticError::FunctionNotDeclared)
    //    }
    //}

    fn analyze_one(&mut self, expr: &Expr) -> Result<Type, SemanticError> {
        match expr {
            Expr::Arithmetic(a, (op, _), b) => self.analyze_arith(a, op, b),
            Expr::Comparation(a, (op, _), b) => self.analyze_comp(a, op, b),
            Expr::Logical(a, (op, _), b) => self.analyze_logical(a, op, b),
            Expr::Unary((op, _), a) => self.analyze_unary(op, a),
            Expr::Grouping(expr) => self.analyze_one(expr),
            Expr::Literal((value, _)) => Ok(self.analyze_literal(value)),
            Expr::Variable(_, identifier) => self.analyze_var_expr(identifier),
            Expr::Call(callee, args) => self.analyze_call_expr(callee, args),
        }
    }

    fn with_new_env<T>(&mut self, fun: impl Fn(&mut Self) -> T) -> T {
        self.symbol_table.push(HashMap::default());
        let result = fun(self);
        self.symbol_table.pop();
        result
    }

    fn analyze_var_expr(&mut self, identifier: &str) -> Result<Type, SemanticError> {
        for env in self.symbol_table.iter().rev() {
            if let Some(t) = env.get(identifier) {
                return Ok(t.clone());
            }
        }

        Err(SemanticError::VariableNotDeclared)
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
            Value::Fun(fun) => Type::Fun(fun.params_types(), *fun.return_type()),
        }
    }

    fn analyze_call_expr(&mut self, callee: &Expr, args: &[Expr]) -> Result<Type, SemanticError> {
        if let Type::Fun(params_types, return_type) = self.analyze_one(callee)? {
            if params_types.len() != args.len() {
                return Err(SemanticError::ArityMismatch(params_types.len(), args.len()));
            }

            self.with_new_env(|analyzer| {
                for (arg, param_var_type) in args.iter().zip(&params_types) {
                    let arg_type = analyzer.analyze_one(arg)?;

                    let arg_var_type: VarType = arg_type.clone().into();
                    let param_type: Type = param_var_type.into();

                    if arg_var_type != *param_var_type {
                        return Err(SemanticError::MismatchedTypes(param_type, arg_type, None));
                    }
                }

                Ok(())
            })?;
            Ok((&return_type).into())
        } else {
            Err(SemanticError::FunctionNotDeclared)
        }
    }
}
