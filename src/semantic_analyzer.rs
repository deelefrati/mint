use crate::{
    environment::Environment,
    error::{semantic::SemanticError, Error},
    expr::*,
    mint_type::MintType,
    stmt::Stmt,
    token::Token,
    token_type::VarType,
};
use std::collections::HashMap;

type SmntEnv = Environment<(Type, bool)>;

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => write!(f, "Null"),
            Type::Num => write!(f, "Number"),
            Type::Bool => write!(f, "Boolean"),
            Type::Str => write!(f, "String"),
            Type::Fun(_, _, _, _) => write!(f, "Function"),
            Type::UserType(mint_type) => write!(f, "{}", mint_type.name.lexeme()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Num,
    Bool,
    Null,
    Str,
    Fun(SmntEnv, Vec<VarType>, VarType, Vec<String>),
    UserType(MintType),
}

impl std::convert::From<&VarType> for Type {
    fn from(var_type: &VarType) -> Self {
        match var_type {
            VarType::Number => Type::Num,
            VarType::Boolean => Type::Bool,
            VarType::Null => Type::Null,
            VarType::String => Type::Str,
            VarType::Function => Type::Fun(
                SmntEnv::new(HashMap::default()),
                Vec::default(),
                VarType::Null,
                Vec::default(),
            ),
            VarType::UserType(token) => Type::UserType(MintType::new(token.clone(), &[])),
        }
    }
}
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
    symbol_table: SmntEnv,
    errors: Vec<Error>,
    analyzing_function: bool,
    hoisting: bool,
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
            symbol_table: SmntEnv::new(HashMap::default()),
            errors: vec![],
            analyzing_function: false,
            hoisting: false,
        }
    }

    fn validate_return(&self, body: &[Stmt]) -> bool {
        if body.iter().any(|stmt| matches!(stmt, Stmt::Return(_, _))) {
            true
        } else {
            let if_stmts = body
                .iter()
                .filter_map(|stmt| match stmt {
                    Stmt::IfStmt(_, then, else_) => Some((then, else_)),
                    _ => None,
                })
                .collect::<Vec<(&Vec<Stmt>, &Vec<Stmt>)>>();
            let mut valid = false;
            for (then, else_) in if_stmts.iter().rev() {
                valid |= self.validate_return(then) && self.validate_return(else_);
                if valid {
                    break;
                }
            }
            valid
        }
    }

    //fn declare_env_vars(&mut self, body: &[Stmt]) -> Result<(), SemanticError> {
    //    let vec = body.iter().filter_map(|stmt| match stmt {
    //        Stmt::VarStmt(id, var_type, expr) => Some((id, var_type, expr)),
    //        _ => None,
    //    });
    //    self.hoisting = true;
    //    for (id, var_type, expr) in vec {
    //        if var_type.is_none() {
    //            let evaluated_type = self.analyze_one(expr)?;
    //            self.declare(&id, evaluated_type);
    //        } else {
    //            self.declare(&id, var_type.as_ref().unwrap().into());
    //        }
    //    }
    //    self.hoisting = false;

    //    Ok(())
    //}

    fn hoist_declarations(&mut self, stmts: &'a [Stmt]) -> Vec<&'a Stmt> {
        let mut declarations: Vec<&Stmt> = stmts
            .iter()
            .filter(|stmt| matches!(stmt, Stmt::Function(_, _, _, _, _) | Stmt::TypeStmt(_,_)))
            .collect();
        let mut not_declarations: Vec<&Stmt> = stmts
            .iter()
            .filter(|stmt| !matches!(stmt, Stmt::Function(_, _, _, _, _)| Stmt::TypeStmt(_,_) ))
            .collect();
        declarations.append(&mut not_declarations);
        declarations
    }

    pub fn analyze(
        &mut self,
        stmts: &'a [Stmt],
        fun_ret_type: Option<VarType>,
    ) -> Result<Vec<Stmt>, Vec<Error>> {
        //if let Err(err) = self.declare_env_vars(stmts) {
        //    self.errors.push(Error::Semantic(err));
        //}
        let mut hoisted_stmts = self.hoist_declarations(stmts);
        for stmt in hoisted_stmts.clone() {
            match stmt {
                Stmt::ExprStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) => self.insert(&expr, t),
                    Err(semantic_error) => self.errors.push(Error::Semantic(semantic_error)),
                },
                Stmt::AssertStmt(expr) => match self.analyze_one(&expr) {
                    Ok(t) if !self.compare_types(&t, &Type::Bool) => {
                        let (line, starts_at, ends_at) = expr.placement();
                        self.errors
                            .push(Error::Semantic(SemanticError::MismatchedTypes(
                                line,
                                starts_at,
                                ends_at,
                                Type::Bool,
                                t,
                            )))
                    }
                    Err(error) => self.errors.push(Error::Semantic(error)),
                    _ => {}
                },
                Stmt::PrintStmt(exprs) => {
                    for expr in exprs {
                        if let Err(err) = self.analyze_one(expr) {
                            self.errors.push(Error::Semantic(err));
                        }
                    }
                }
                Stmt::VarStmt(var_name, var_type, expr) => match self.analyze_one(&expr) {
                    Ok(t) => match (var_type, t.clone()) {
                        (Some(VarType::Boolean), Type::Bool) => {
                            if let Some((_, true)) = self.define(var_name, Type::Bool) {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        line,
                                        starts_at,
                                        ends_at,
                                        var_name.clone(),
                                    ),
                                ));
                            }
                        }
                        (Some(VarType::String), Type::Str) => {
                            if let Some((_, true)) = self.define(var_name, Type::Str) {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        line,
                                        starts_at,
                                        ends_at,
                                        var_name.clone(),
                                    ),
                                ));
                            }
                        }
                        (Some(VarType::Number), Type::Num) => {
                            if let Some((_, true)) = self.define(var_name, Type::Num) {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        line,
                                        starts_at,
                                        ends_at,
                                        var_name.clone(),
                                    ),
                                ));
                            }
                        }
                        (Some(VarType::Null), Type::Null) => {
                            if let Some((_, true)) = self.define(var_name, Type::Null) {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        line,
                                        starts_at,
                                        ends_at,
                                        var_name.clone(),
                                    ),
                                ));
                            }
                        }
                        (Some(VarType::UserType(user_type)), Type::UserType(mint_type)) => {
                            if user_type.lexeme() != mint_type.name.lexeme() {
                                self.errors.push(Error::Semantic(
                                    SemanticError::IncompatibleDeclaration(
                                        mint_type.name.line(),
                                        mint_type.name.starts_at(),
                                        mint_type.name.ends_at(),
                                        var_type.clone().unwrap(),
                                        t.clone(),
                                    ),
                                ));
                            }
                            if let Some((_, true)) =
                                self.define(var_name, Type::UserType(mint_type))
                            {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        line,
                                        starts_at,
                                        ends_at,
                                        var_name.clone(),
                                    ),
                                ));
                            }
                        }
                        (None, _) => {
                            if let Some((_, true)) = self.define(var_name, t) {
                                let (line, starts_at, ends_at) = expr.placement();
                                self.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        line,
                                        starts_at,
                                        ends_at,
                                        var_name.clone(),
                                    ),
                                ));
                            }
                        }
                        (Some(expected), found) => {
                            let (line, starts_at, ends_at) = expr.placement();
                            self.errors.push(Error::Semantic(
                                SemanticError::IncompatibleDeclaration(
                                    line,
                                    starts_at,
                                    ends_at,
                                    expected.clone(),
                                    found,
                                ),
                            ))
                        }
                    },
                    Err(semantic_error) => self.errors.push(Error::Semantic(semantic_error)),
                },
                Stmt::Block(stmts) => self.with_new_env(|analyzer| {
                    if let Err(nested_errors) = analyzer.analyze(stmts, fun_ret_type.clone()) {
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
                        analyzer.analyze(then, fun_ret_type.clone()).ok();
                    });
                    self.with_new_env(|analyzer| {
                        analyzer.analyze(else_, fun_ret_type.clone()).ok();
                    });
                }
                Stmt::Function(token, params, body, return_type, return_token) => {
                    let param_types = params
                        .iter()
                        .map(|(_, param_type)| param_type.clone())
                        .collect::<Vec<VarType>>();
                    self.declare(
                        &token.lexeme(),
                        Type::Fun(
                            SmntEnv::new(HashMap::default()),
                            param_types.clone(),
                            return_type.clone(),
                            vec![],
                        ),
                    );
                    let (fun_env, declared_keys) = self.with_new_env(|analyzer| {
                        analyzer.analyzing_function = true;
                        for (param, type_) in params {
                            if analyzer.define(&param.lexeme(), type_.into()).is_some() {
                                analyzer.errors.push(Error::Semantic(
                                    SemanticError::VariableOverwrited(
                                        param.line(),
                                        param.starts_at(),
                                        param.ends_at(),
                                        param.lexeme(),
                                    ),
                                ));
                            }
                        }
                        analyzer.analyze(body, Some(return_type.clone())).ok();

                        (
                            analyzer.symbol_table.clone(),
                            analyzer.symbol_table.declared_keys(),
                        )
                    });
                    if let Some((_, true)) = self.define(
                        &token.lexeme(),
                        Type::Fun(
                            fun_env.clone(),
                            param_types.clone(),
                            return_type.clone(),
                            declared_keys.clone(),
                        ),
                    ) {
                        self.errors
                            .push(Error::Semantic(SemanticError::VariableOverwrited(
                                token.line(),
                                token.starts_at(),
                                token.ends_at(),
                                token.lexeme(),
                            ))); // FIXME trocar o erro
                    }
                    self.analyzing_function = false;

                    if return_type != &VarType::Null && !self.validate_return(body) {
                        self.errors
                            .push(Error::Semantic(SemanticError::MissingReturnStmt(
                                return_token.as_ref().unwrap().line(),
                                return_token.as_ref().unwrap().starts_at(),
                                return_token.as_ref().unwrap().ends_at(),
                            )))
                    }
                }
                Stmt::Return(token, expr) => match expr {
                    Some(expr) => {
                        if let Some(fun_ret_t) = fun_ret_type.clone() {
                            match self.analyze_one(&expr) {
                                Ok(t) => {
                                    if !self.compare_types(&(&fun_ret_t).into(), &t) {
                                        self.errors.push(Error::Semantic(
                                            SemanticError::MismatchedTypes(
                                                token.line(),
                                                token.starts_at(),
                                                token.ends_at(),
                                                (&fun_ret_t).into(),
                                                t.clone(),
                                            ),
                                        ));
                                    }
                                    self.insert(&expr, t);
                                }

                                Err(semantic_err) => {
                                    self.errors.push(Error::Semantic(semantic_err))
                                }
                            }
                        } else {
                            self.errors
                                .push(Error::Semantic(SemanticError::ReturnTopLever(
                                    token.line(),
                                    token.starts_at(),
                                    token.ends_at(),
                                )))
                        }
                    }
                    None => {
                        if let Some(fun_ret_t) = fun_ret_type.clone() {
                            if !self.compare_types(&(&fun_ret_t).into(), &Type::Null) {
                                self.errors
                                    .push(Error::Semantic(SemanticError::MismatchedTypes(
                                        token.line(),
                                        token.starts_at(),
                                        token.ends_at(),
                                        Type::Null,
                                        (&fun_ret_t).into(),
                                    )))
                            }
                        }
                    }
                },
                Stmt::TypeStmt(token, attrs) => {
                    if let Some((_, true)) = self.define(
                        &token.lexeme(),
                        Type::UserType(MintType::new(token.clone(), attrs)),
                    ) {
                        self.errors
                            .push(Error::Semantic(SemanticError::TypeAlreadyDeclared(
                                token.line(),
                                token.starts_at(),
                                token.ends_at(),
                            )))
                    }
                }
            }
        }

        if self.errors.is_empty() {
            #[allow(clippy::map_clone)]
            let x: Vec<Stmt> = hoisted_stmts.iter_mut().map(|s| s.clone()).collect();
            Ok(x)
        } else {
            Err(self.errors.clone())
        }
    }

    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn declare(&mut self, id: &str, t: Type) {
        self.symbol_table.define(id.to_string(), (t, false));
    }

    fn define(&mut self, id: &str, t: Type) -> Option<(Type, bool)> {
        self.symbol_table.define(id.to_string(), (t, true))
    }

    fn analyze_one(&mut self, expr: &Expr) -> Result<Type, SemanticError> {
        match expr {
            Expr::Arithmetic(a, (op, _), b) => self.analyze_arith(a, op, b, expr),
            Expr::Comparation(a, (op, _), b) => self.analyze_comp(a, op, b, expr),
            Expr::Logical(a, (op, _), b) => self.analyze_logical(a, op, b, expr),
            Expr::Unary((op, _), a) => self.analyze_unary(op, a, expr),
            Expr::Grouping(expr) => self.analyze_one(expr),
            Expr::Literal((value, _)) => Ok(self.analyze_literal(value)),
            Expr::Variable(token, identifier) => self.analyze_var_expr(token, identifier),
            Expr::Call(callee, args) => self.analyze_call_expr(callee, args, expr),
            Expr::Instantiate(type_token, attributes) => {
                self.analyze_instatiation(type_token, attributes)
            }
            Expr::Get(expr, token) => {
                let expr_type = self.analyze_one(expr)?;
                if let Type::UserType(user_type) = expr_type.clone() {
                    if let Some(Type::UserType(complete_type)) =
                        self.get_var(&user_type.name.lexeme())
                    {
                        if let Some((_, var_type)) = complete_type
                            .attrs
                            .iter()
                            .find(|(t, _)| t.to_string() == token.lexeme())
                        {
                            return Ok(var_type.into());
                        }
                    }
                }
                Err(SemanticError::PropertyDoesNotExist(
                    token.line(),
                    token.starts_at(),
                    token.ends_at(),
                    token.lexeme(),
                    expr_type,
                ))
            }
        }
    }

    fn analyze_instatiation(
        &mut self,
        token: &Token,
        attributes: &[(Token, Expr)],
    ) -> Result<Type, SemanticError> {
        if let Some(type_) = self.get_var(&token.lexeme()) {
            match type_.clone() {
                Type::UserType(mint_type) => {
                    for (token, expr) in attributes {
                        let expr_type = self.analyze_one(expr)?;
                        match mint_type.attrs.get(&token.lexeme()) {
                            Some(attr_type) => {
                                if !self.compare_types(&expr_type, &attr_type.into()) {
                                    let (line, starts_at, ends_at) = expr.placement();
                                    return Err(SemanticError::MismatchedTypes(
                                        line,
                                        starts_at,
                                        ends_at,
                                        attr_type.into(),
                                        expr_type,
                                    ));
                                }
                            }
                            None => {
                                return Err(SemanticError::PropertyDoesNotExist(
                                    token.line(),
                                    token.starts_at(),
                                    token.starts_at(),
                                    token.lexeme(),
                                    type_,
                                ))
                            }
                        }
                    }
                    Ok(Type::UserType(mint_type))
                }
                _ => Err(SemanticError::TypeNotIntantiable(
                    token.line(),
                    token.starts_at(),
                    token.ends_at(),
                    token.lexeme(),
                )),
            }
        } else {
            Err(SemanticError::TypeNotDeclared(
                token.line(),
                token.starts_at(),
                token.ends_at(),
                token.lexeme(),
            ))
        }
    }

    fn with_new_env<T>(&mut self, fun: impl Fn(&mut Self) -> T) -> T {
        self.symbol_table.push();
        let result = fun(self);
        self.symbol_table.pop();
        result
    }

    fn analyze_var_expr(&mut self, token: &Token, identifier: &str) -> Result<Type, SemanticError> {
        if let Some(t) = self.get_var(identifier) {
            Ok(t)
        } else {
            Err(SemanticError::VariableNotDeclared(
                token.line(),
                token.starts_at(),
                token.ends_at(),
                token.lexeme(),
            ))
        }
    }

    fn analyze_unary(
        &mut self,
        op: &UnaryOp,
        expr: &Expr,
        original_expr: &Expr,
    ) -> Result<Type, SemanticError> {
        use UnaryOp::*;
        let expr_type = self.analyze_one(expr)?;

        let t = match (op, expr_type) {
            (Bang, Type::Bool) => Type::Bool,
            (Minus, Type::Num) => Type::Num,
            (Plus, Type::Num) => Type::Num,
            (op, t) => {
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SemanticError::IncompatibleUnaryOp(
                    line, starts_at, ends_at, *op, t,
                ));
            }
        };
        Ok(t)
    }

    fn analyze_arith(
        &mut self,
        a: &Expr,
        op: &ArithmeticOp,
        b: &Expr,
        original_expr: &Expr,
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
            (op, left, right) => {
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SemanticError::IncompatibleArith(
                    line, starts_at, ends_at, *op, left, right,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_logical(
        &mut self,
        a: &Expr,
        op: &LogicalOp,
        b: &Expr,
        original_expr: &Expr,
    ) -> Result<Type, SemanticError> {
        use LogicalOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Bool, Type::Bool) => Type::Bool,
            (Or, Type::Bool, Type::Bool) => Type::Bool,

            (op, left, right) => {
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SemanticError::IncompatibleLogicOp(
                    line, starts_at, ends_at, *op, left, right,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_comp(
        &mut self,
        a: &Expr,
        op: &ComparationOp,
        b: &Expr,
        original_expr: &Expr,
    ) -> Result<Type, SemanticError> {
        use ComparationOp::*;
        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;
        let expr_type = match (op, type_a, type_b) {
            (StrictNotEqual, Type::Num, Type::Num) => Type::Bool,
            (StrictNotEqual, Type::Bool, Type::Bool) => Type::Bool,
            (StrictNotEqual, Type::Str, Type::Str) => Type::Bool,
            (StrictNotEqual, _, Type::Null) => Type::Bool,
            (StrictNotEqual, Type::Null, _) => Type::Bool,
            (StrictNotEqual, Type::UserType(_), Type::UserType(_)) => Type::Bool,

            (StrictEqual, Type::Num, Type::Num) => Type::Bool,
            (StrictEqual, Type::Bool, Type::Bool) => Type::Bool,
            (StrictEqual, Type::Str, Type::Str) => Type::Bool,
            (StrictEqual, Type::UserType(_), Type::UserType(_)) => Type::Bool,
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
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SemanticError::IncompatibleComparation(
                    line, starts_at, ends_at, *op, left, right,
                ));
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
            Value::Fun(fun) => Type::Fun(
                SmntEnv::new(HashMap::default()),
                fun.params_types(),
                fun.return_type().clone(),
                vec![],
            ),
            Value::Type(_) => Type::Null,
            Value::TypeInstance(_) => Type::Null,
        }
    }

    fn analyze_call_expr(
        &mut self,
        callee: &Expr,
        args: &[Expr],
        original_expr: &Expr,
    ) -> Result<Type, SemanticError> {
        if let Type::Fun(env, params_types, return_type, declared_keys) =
            self.analyze_one(callee)?
        {
            if params_types.len() != args.len() {
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SemanticError::ArityMismatch(
                    line,
                    starts_at,
                    ends_at,
                    params_types.len(),
                    args.len(),
                ));
            } else if !self.analyzing_function {
                let old_env = std::mem::replace(&mut self.symbol_table, env);
                for (arg, param_var_type) in args.iter().zip(&params_types) {
                    let arg_type = self.analyze_one(arg)?;

                    let param_type: Type = param_var_type.into();

                    if !self.compare_types(&arg_type, &param_type) {
                        let (line, (starts_at, ends_at)) =
                            (arg.get_line(), arg.get_expr_placement());
                        return Err(SemanticError::MismatchedTypes(
                            line, starts_at, ends_at, param_type, arg_type,
                        ));
                    }
                }
                for key in &declared_keys {
                    if self.get_var(&key).is_none() {
                        let (line, starts_at, ends_at) = callee.placement();
                        let fun_name = callee.get_token().lexeme();
                        self.errors.push(Error::Semantic(SemanticError::UnboundVar(
                            line,
                            starts_at,
                            ends_at,
                            key.to_string(),
                            fun_name,
                        )));
                    }
                }
                self.symbol_table = old_env;
            }
            Ok((&return_type).into())
        } else {
            let (line, starts_at, ends_at) = callee.placement();
            Err(SemanticError::FunctionNotDeclared(line, starts_at, ends_at))
        }
    }

    fn get_var(&mut self, id: &str) -> Option<Type> {
        let x = self.symbol_table.get(id);

        if let Some((var_type, defined)) = x {
            if !self.analyzing_function && !self.hoisting {
                if defined {
                    Some(var_type)
                } else {
                    None
                }
            } else {
                Some(var_type)
            }
        } else {
            None
        }
    }

    fn compare_types(&self, x: &Type, y: &Type) -> bool {
        match (x, y) {
            (Type::UserType(a), Type::UserType(b)) => a.name.lexeme() == b.name.lexeme(),
            _ => x == y,
        }
    }
}
