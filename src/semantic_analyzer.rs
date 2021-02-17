use crate::{
    environment::Environment,
    error::{semantic::SemanticError, Error},
    expr::*,
    mint_type::MintType,
    stmt::Stmt,
    token::Token,
    token_type::{Literal, VarType},
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
            Type::Literals(literal) => write!(f, "{}", literal),
            Type::Fun(_, _, _, _) => write!(f, "Function"),
            Type::UserType(mint_type) => write!(f, "{}", mint_type.name.lexeme()),
            Type::Union(_) => write!(f, "Union"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Num,
    Bool,
    Null,
    Str,
    Literals(Literal),
    Fun(SmntEnv, Vec<VarType>, VarType, Vec<String>),
    UserType(MintType),
    Union(Vec<(Type, Token)>),
}

impl std::convert::From<&VarType> for Type {
    fn from(var_type: &VarType) -> Self {
        match var_type {
            VarType::Number => Type::Num,
            VarType::Boolean => Type::Bool,
            VarType::Null => Type::Null,
            VarType::String => Type::Str,
            VarType::Literals(literal) => Type::Literals(literal.to_owned()),
            VarType::Function => Type::Fun(
                SmntEnv::new(HashMap::default()),
                Vec::default(),
                VarType::Null,
                Vec::default(),
            ),
            VarType::UserType(token) => {
                Type::UserType(MintType::new(token.clone(), HashMap::default()))
            }
            VarType::Union(var_types) => Type::Union(
                var_types
                    .iter()
                    .map(|(var_type, token)| (var_type.into(), token.clone()))
                    .collect(),
            ),
        }
    }
}
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
    symbol_table: SmntEnv,
    errors: Vec<Error>,
    analyzing_function: Vec<bool>,
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
            analyzing_function: vec![],
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
            let block_stmt = body
                .iter()
                .filter_map(|stmt| match stmt {
                    Stmt::Block(body) => Some(body),
                    _ => None,
                })
                .collect::<Vec<&Vec<Stmt>>>();
            let mut if_stmt_valid = false;
            let mut block_valid = false;
            for (then, else_) in if_stmts.iter().rev() {
                if_stmt_valid |= self.validate_return(then) && self.validate_return(else_);
                if if_stmt_valid {
                    break;
                }
            }
            for stmt in block_stmt.iter().rev() {
                block_valid |= self.validate_return(stmt);
                if block_valid {
                    break;
                }
            }
            if_stmt_valid || block_valid
        }
    }

    fn declare_env_vars(&mut self, body: &[Stmt]) -> Result<(), SemanticError> {
        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::VarStmt(id, var_type, expr) => Some((id, var_type, expr)),
            _ => None,
        });
        self.hoisting = true;
        for (id, var_type, expr) in vec {
            if var_type.is_none() {
                match expr {
                    Expr::Call(callee, _) => {
                        if let Some(Type::Fun(_, _, ret_type, _)) =
                            self.get_var(&callee.get_token().lexeme())
                        {
                            self.declare(&id, (&ret_type).into());
                        }
                    }
                    _ => {
                        let type_expr = self.analyze_one(expr)?;
                        self.declare(&id, type_expr)
                    }
                }
            } else {
                self.declare(&id, var_type.as_ref().unwrap().into());
            }
        }
        self.hoisting = false;

        Ok(())
    }

    fn hoist_declarations(&mut self, stmts: &'a [Stmt]) -> Vec<&'a Stmt> {
        let mut declarations: Vec<&Stmt> = stmts
            .iter()
            .filter(|stmt| matches!(stmt, Stmt::Function(_, _, _, _, _) | Stmt::TypeStmt(_, _)))
            .collect();
        declarations.iter().for_each(|decl| match decl {
            Stmt::Function(token, params, _, ret_type, _) => self.declare(
                &token.lexeme(),
                Type::Fun(
                    SmntEnv::new(HashMap::default()),
                    params.iter().map(|(_, t)| t.clone()).collect(),
                    ret_type.clone(),
                    vec![],
                ),
            ),
            Stmt::TypeStmt(token, attrs) => self.declare(
                &token.lexeme(),
                Type::UserType(MintType::new(token.clone(), attrs.clone())),
            ),
            _ => (),
        });
        let mut not_declarations: Vec<&Stmt> = stmts
            .iter()
            .filter(|stmt| !matches!(stmt, Stmt::Function(_, _, _, _, _) | Stmt::TypeStmt(_, _)))
            .collect();
        declarations.append(&mut not_declarations);
        declarations
    }

    pub fn analyze(
        &mut self,
        stmts: &'a [Stmt],
        fun_ret_type: Option<VarType>,
    ) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut hoisted_stmts = self.hoist_declarations(stmts);

        if let Err(err) = self.declare_env_vars(stmts) {
            self.errors.push(Error::Semantic(err));
        }

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
                        (Some(VarType::Boolean), Type::Literals(Literal::Boolean(_))) => {
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
                        (Some(VarType::String), Type::Literals(Literal::String(_))) => {
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
                        (Some(VarType::Number), Type::Literals(Literal::Number(_))) => {
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
                        (Some(VarType::Literals(_)), Type::Literals(_)) => {
                            if self.compare_types(&t, &var_type.as_ref().unwrap().into()) {
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
                            } else {
                                let (line, starts_at, ends_at) = expr.placement();
                                let expected: VarType = var_type.clone().unwrap();
                                self.errors.push(Error::Semantic(
                                    SemanticError::IncompatibleDeclaration(
                                        line,
                                        starts_at,
                                        ends_at,
                                        expected,
                                        t.clone(),
                                    ),
                                ))
                            }
                        }
                        (None, Type::Literals(literal)) => {
                            if let Some((_, true)) = self.define(var_name, literal.to_primitive()) {
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
                        (Some(VarType::Union(union)), Type::Literals(literal)) => {
                            if self.compare_types(&t, &var_type.as_ref().unwrap().into()) {
                                let res = union.iter().any(|(var_type, _)| match var_type {
                                    VarType::Literals(lit) => lit.clone() == literal,
                                    _ => false,
                                });
                                if let Some((_, true)) = self.define(
                                    var_name,
                                    if res {
                                        t.clone()
                                    } else {
                                        literal.to_primitive()
                                    },
                                ) {
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
                            } else {
                                let (line, starts_at, ends_at) = expr.placement();
                                let expected: VarType = var_type.clone().unwrap();
                                self.errors.push(Error::Semantic(
                                    SemanticError::IncompatibleDeclaration(
                                        line,
                                        starts_at,
                                        ends_at,
                                        expected,
                                        t.clone(),
                                    ),
                                ))
                            }
                        }
                        (Some(VarType::Union(_)), _) => {
                            if self.compare_types(&t, &var_type.as_ref().unwrap().into()) {
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
                            } else {
                                let (line, starts_at, ends_at) = expr.placement();
                                let expected: VarType = var_type.clone().unwrap();
                                self.errors.push(Error::Semantic(
                                    SemanticError::IncompatibleDeclaration(
                                        line,
                                        starts_at,
                                        ends_at,
                                        expected,
                                        t.clone(),
                                    ),
                                ))
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
                        analyzer.analyzing_function.push(true);
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
                        let mut declared_keys = vec![];
                        analyzer.declared_keys(body, &mut declared_keys);
                        (analyzer.symbol_table.clone(), declared_keys)
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
                    self.analyzing_function.pop();

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
                                    if !self.compare_types(&t, &(&fun_ret_t).into()) {
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
                        Type::UserType(MintType::new(token.clone(), attrs.clone())),
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
        //println!("{:#?}", self.symbol_table);
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
            Expr::Instantiate(t, args) => self.analyze_instatiation(t, args),
            Expr::Get(expr, token) => self.analyze_get(expr, token),
        }
    }

    fn analyze_get(&mut self, expr: &Box<Expr>, token: &Token) -> Result<Type, SemanticError> {
        let expr_type = self.analyze_one(expr)?;
        if let Type::UserType(user_type) = expr_type.clone() {
            if let Some(Type::UserType(complete_type)) = self.get_var(&user_type.name.lexeme()) {
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

    fn analyze_instatiation(
        &mut self,
        t: &Token,
        args: &[(Token, Expr)],
    ) -> Result<Type, SemanticError> {
        if let Some(type_) = self.get_var(&t.lexeme()) {
            match type_.clone() {
                Type::UserType(mint_type) => {
                    self.instance_to_user_type(args, &mint_type.attrs, &type_)?;
                    Ok(Type::UserType(mint_type))
                }
                Type::Union(union) => {
                    let mut attrs = HashMap::default();
                    if let Some(_) = union.iter().find(|(type_, token)| match type_ {
                        Type::UserType(mint_type) => {
                            attrs = mint_type.attrs.clone();
                            mint_type.name.lexeme() == token.lexeme()
                        }
                        _ => false,
                    }) {
                        self.instance_to_user_type(args, &attrs, &type_)?;
                        Ok(Type::UserType(MintType::new(t.clone(), attrs)))
                    } else {
                        Err(SemanticError::TypeNotAssignable(
                            t.line(),
                            t.starts_at(),
                            t.ends_at(),
                            t.lexeme(),
                            type_,
                        ))
                    }
                }
                _ => Err(SemanticError::TypeNotIntantiable(
                    t.line(),
                    t.starts_at(),
                    t.ends_at(),
                    t.lexeme(),
                )),
            }
        } else {
            Err(SemanticError::TypeNotDeclared(
                t.line(),
                t.starts_at(),
                t.ends_at(),
                t.lexeme(),
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
            (Bang, Type::Literals(Literal::Boolean(_))) => Type::Bool,
            (Minus, Type::Num) => Type::Num,
            (Minus, Type::Literals(Literal::Number(_))) => Type::Num,
            (Plus, Type::Num) => Type::Num,
            (Plus, Type::Literals(Literal::Number(_))) => Type::Num,
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
            (Add, Type::Str, Type::Str) => Type::Str,
            (Add, Type::Literals(Literal::String(_)), Type::Literals(Literal::String(_))) => {
                Type::Str
            }
            (Add, Type::Str, Type::Literals(Literal::String(_))) => Type::Str,
            (Add, Type::Literals(Literal::String(_)), Type::Str) => Type::Str,
            (_, Type::Literals(Literal::Number(_)), Type::Literals(Literal::Number(_))) => {
                Type::Num
            }
            (_, Type::Literals(Literal::Number(_)), Type::Num) => Type::Num,
            (_, Type::Num, Type::Literals(Literal::Number(_))) => Type::Num,
            (_, Type::Num, Type::Num) => Type::Num,

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
        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (_, Type::Literals(Literal::Boolean(_)), Type::Literals(Literal::Boolean(_))) => {
                Type::Bool
            }
            (_, Type::Bool, Type::Literals(Literal::Boolean(_))) => Type::Bool,
            (_, Type::Literals(Literal::Boolean(_)), Type::Bool) => Type::Bool,
            (_, Type::Bool, Type::Bool) => Type::Bool,

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
        fn cmp_eq(
            expr: &Expr,
            op: &ComparationOp,
            left: Type,
            right: Type,
        ) -> Result<Type, SemanticError> {
            let t = match (left.clone(), right.clone()) {
                (Type::Num, Type::Num) => Type::Bool,
                (Type::Literals(Literal::Number(_)), Type::Literals(Literal::Number(_))) => {
                    Type::Bool
                }
                (Type::Num, Type::Literals(Literal::Number(_))) => Type::Bool,
                (Type::Literals(Literal::Number(_)), Type::Num) => Type::Bool,

                (Type::Str, Type::Str) => Type::Bool,
                (Type::Literals(Literal::String(_)), Type::Literals(Literal::String(_))) => {
                    Type::Bool
                }
                (Type::Str, Type::Literals(Literal::String(_))) => Type::Bool,
                (Type::Literals(Literal::String(_)), Type::Str) => Type::Bool,

                (Type::Bool, Type::Bool) => Type::Bool,
                (Type::Literals(Literal::Boolean(_)), Type::Literals(Literal::Boolean(_))) => {
                    Type::Bool
                }
                (Type::Literals(Literal::Boolean(_)), Type::Bool) => Type::Bool,
                (Type::Bool, Type::Literals(Literal::Boolean(_))) => Type::Bool,

                (_, Type::Null) => Type::Bool,
                (Type::Null, _) => Type::Bool,
                (Type::UserType(_), Type::UserType(_)) => Type::Bool,
                _ => {
                    let (line, starts_at, ends_at) = expr.placement();
                    let (l, r) = match (left, right) {
                        (Type::Literals(a), Type::Literals(b)) => {
                            (a.to_primitive(), b.to_primitive())
                        }
                        (Type::Literals(literal), t) => (literal.to_primitive(), t),
                        (t, Type::Literals(literal)) => (t, literal.to_primitive()),
                        (a, b) => (a, b),
                    };
                    return Err(SemanticError::IncompatibleComparation(
                        line, starts_at, ends_at, *op, l, r,
                    ));
                }
            };
            Ok(t)
        }
        fn cmp(
            expr: &Expr,
            op: &ComparationOp,
            left: Type,
            right: Type,
        ) -> Result<Type, SemanticError> {
            let t = match (left.clone(), right.clone()) {
                (Type::Num, Type::Num) => Type::Bool,
                (Type::Str, Type::Str) => Type::Bool,
                (Type::Literals(Literal::Number(_)), Type::Literals(Literal::Number(_))) => {
                    Type::Bool
                }
                (Type::Literals(Literal::Number(_)), Type::Num) => Type::Bool,
                (Type::Num, Type::Literals(Literal::Number(_))) => Type::Bool,
                (Type::Literals(Literal::String(_)), Type::Literals(Literal::String(_))) => {
                    Type::Bool
                }
                (Type::Literals(Literal::String(_)), Type::Str) => Type::Bool,
                (Type::Str, Type::Literals(Literal::String(_))) => Type::Bool,
                _ => {
                    let (line, starts_at, ends_at) = expr.placement();
                    return Err(SemanticError::IncompatibleComparation(
                        line, starts_at, ends_at, *op, left, right,
                    ));
                }
            };
            Ok(t)
        }

        use ComparationOp::*;
        let left = self.analyze_one(a)?;
        let right = self.analyze_one(b)?;
        let expr_type = match op {
            StrictEqual | StrictNotEqual => cmp_eq(original_expr, op, left, right)?,

            LessThan | LessEqual | GreaterThan | GreaterEqual => {
                cmp(original_expr, op, left, right)?
            }
        };
        Ok(expr_type)
    }

    fn analyze_literal(&self, value: &Value) -> Type {
        match value {
            Value::Null => Type::Null,
            Value::Boolean(boolean) => Type::Literals(Literal::Boolean(*boolean)),
            Value::Number(num) => Type::Literals(Literal::Number(*num)),
            Value::Str(string) => Type::Literals(Literal::String(string.to_string())),
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
            } else if self.analyzing_function.is_empty() {
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
            if self.analyzing_function.is_empty() && !self.hoisting {
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

    fn compare_types(&self, found: &Type, expected: &Type) -> bool {
        println!("{:?} \n {:?}", found, expected);
        println!();
        match (found, expected) {
            (Type::UserType(a), Type::UserType(b)) => a.name.lexeme() == b.name.lexeme(),
            (Type::Literals(a), Type::Literals(b)) => a == b,
            (Type::Literals(Literal::Number(_)), Type::Num) => true,
            (Type::Literals(Literal::String(_)), Type::Str) => true,
            (Type::Literals(Literal::Boolean(_)), Type::Bool) => true,
            (_, Type::Union(union)) => union.iter().any(|(t, _)| self.compare_types(found, t)),
            _ => found == expected,
        }
    }

    fn declared_keys(&mut self, stmts: &'a [Stmt], declared_keys: &mut Vec<String>) {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(expr) => declared_keys.append(&mut self.expr_keys(expr)),
                Stmt::AssertStmt(expr) => declared_keys.append(&mut self.expr_keys(expr)),
                Stmt::PrintStmt(exprs) => {
                    for expr in exprs {
                        declared_keys.append(&mut self.expr_keys(expr));
                    }
                }
                Stmt::VarStmt(name, _, expr) => {
                    declared_keys.push(name.to_string());
                    declared_keys.append(&mut self.expr_keys(expr));
                }
                Stmt::Block(body) => self.declared_keys(body, declared_keys),
                Stmt::IfStmt(expr, then, else_) => {
                    declared_keys.append(&mut self.expr_keys(expr));
                    self.declared_keys(then, declared_keys);
                    self.declared_keys(else_, declared_keys);
                }
                Stmt::Function(token, params, _, _, _) => {
                    declared_keys.push(token.lexeme());
                    for (token, _) in params {
                        declared_keys.push(token.lexeme());
                    }
                }
                Stmt::Return(_, expr) => {
                    if let Some(expr) = expr {
                        declared_keys.append(&mut self.expr_keys(expr));
                    }
                }
                Stmt::TypeStmt(token, _) => declared_keys.push(token.lexeme()),
            }
        }
    }

    fn expr_keys(&mut self, expr: &Expr) -> Vec<String> {
        match expr {
            Expr::Arithmetic(l, _, r) => {
                let mut l_name = self.expr_keys(l);
                let mut r_name = self.expr_keys(r);
                l_name.append(&mut r_name);
                l_name
            }
            Expr::Comparation(l, _, r) => {
                let mut l_name = self.expr_keys(l);
                let mut r_name = self.expr_keys(r);
                l_name.append(&mut r_name);
                l_name
            }
            Expr::Logical(l, _, r) => {
                let mut l_name = self.expr_keys(l);
                let mut r_name = self.expr_keys(r);
                l_name.append(&mut r_name);
                l_name
            }
            Expr::Unary(_, r) => self.expr_keys(r),
            Expr::Grouping(expr) => self.expr_keys(expr),
            Expr::Literal(_) => vec![],
            Expr::Variable(token, _) => vec![token.lexeme()],
            Expr::Call(callee, _params) => self.expr_keys(callee),
            Expr::Instantiate(t, _) => vec![t.lexeme()],
            Expr::Get(_, token) => vec![token.lexeme()],
        }
    }

    //fn union_into(
    //    &mut self,
    //    union: &[(VarType, Token)],
    //) -> Result<Vec<(Type, Token)>, SemanticError> {
    //    let new_union: Result<Vec<(Type, Token)>, _> = union
    //        .iter()
    //        .map(|(var_type, token)| match var_type {
    //            VarType::UserType(t) => {
    //                if let Some(user_type) = self.get_var(&t.lexeme()) {
    //                    Ok((user_type, token.clone()))
    //                } else {
    //                    Err(SemanticError::TypeNotDeclared(
    //                        t.line(),
    //                        t.starts_at(),
    //                        t.ends_at(),
    //                        t.lexeme(),
    //                    ))
    //                }
    //            }
    //            type_ => Ok((type_.into(), token.clone())),
    //        })
    //        .collect();
    //    new_union
    //}

    fn instance_to_user_type(
        &mut self,
        args: &[(Token, Expr)],
        attrs: &HashMap<String, VarType>,
        type_: &Type,
    ) -> Result<(), SemanticError> {
        for (token, expr) in args {
            let expr_type = self.analyze_one(expr)?;
            match attrs.get(&token.lexeme()) {
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
                        type_.clone(),
                    ))
                }
            }
        }
        Ok(())
    }
}
