use crate::{
    environment::Environment,
    error::{runtime::RuntimeError, Error},
    expr::*,
    mint_type::MintType,
    stmt::*,
    token::Token,
};
use std::collections::HashMap;

type InterpreterResult = Result<Value, RuntimeError>;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment<Value>,
}

impl Interpreter {
    fn binary_arith_op(
        &self,
        left: &Value,
        op_and_token: &OpWithToken<ArithmeticOp>,
        right: &Value,
    ) -> InterpreterResult {
        use ArithmeticOp::*;
        use Value::*;

        let (op, token) = op_and_token;

        match (op, left, right) {
            (Sub, Number(a), Number(b)) => Ok(Number(*a - *b)),
            (Add, Number(a), Number(b)) => Ok(Number(*a + *b)),
            (Div, Number(a), Number(b)) => {
                if *b != 0.0 {
                    Ok(Value::Number(a / b))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.line(),
                        token.starts_at(),
                        token.ends_at(),
                    ))
                }
            }
            (Mul, Number(a), Number(b)) => Ok(Number(*a * *b)),
            (Mod, Number(a), Number(b)) => Ok(Number(*a % *b)),
            // FIXME panic will never happen
            _ => panic!("interpreter::binary_arith_op failed unexpectedly"),
        }
    }

    fn eval_binary_arith_expr(
        &mut self,
        left: &Expr,
        op_and_token: &OpWithToken<ArithmeticOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        self.binary_arith_op(&eval_left, op_and_token, &eval_right)
    }

    fn unary_op(&self, op_and_token: &OpWithToken<UnaryOp>, right: &Value) -> InterpreterResult {
        use UnaryOp::*;
        use Value::*;

        let (op, _token) = op_and_token;

        // FIXME panic will never happen
        match (op, right) {
            (Minus, Number(a)) => Ok(Number(-*a)),
            (Plus, Number(a)) => Ok(Number(*a)),
            (Bang, Boolean(a)) => Ok(Boolean(!*a)),
            _ => panic!("interpreter::unary_op failed unexpectedly"),
        }
    }

    fn eval_unary_expr(
        &mut self,
        op_and_token: &OpWithToken<UnaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let eval_right = self.eval_expr(right)?;

        self.unary_op(op_and_token, &eval_right)
    }

    fn eval_comp_expr(
        &mut self,
        left: &Expr,
        op_and_token: &OpWithToken<ComparationOp>,
        right: &Expr,
    ) -> InterpreterResult {
        use Value::*;
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        let (op, _) = op_and_token;
        let error_margin = f64::EPSILON; // Use an epsilon for comparison

        // TODO ver se realmente vai utilizar o Equal
        let result = match (op, eval_left, eval_right) {
            (ComparationOp::StrictEqual, Number(a), Number(b)) => {
                Boolean((a - b).abs() < error_margin) // a == b to make clippy happy
            }
            (ComparationOp::StrictEqual, Boolean(a), Boolean(b)) => Boolean(a == b),
            (ComparationOp::StrictEqual, Str(a), Str(b)) => Boolean(a == b),
            (ComparationOp::StrictEqual, Null, Null) => Boolean(true),
            (ComparationOp::StrictEqual, _, Null) => Boolean(false),
            (ComparationOp::StrictEqual, Null, _) => Boolean(false),

            (ComparationOp::StrictNotEqual, Number(a), Number(b)) => {
                Boolean((a - b).abs() > error_margin) // a != b to make clippy happy
            }
            (ComparationOp::StrictNotEqual, Boolean(a), Boolean(b)) => Boolean(a != b),
            (ComparationOp::StrictNotEqual, Str(a), Str(b)) => Boolean(a != b),
            (ComparationOp::StrictNotEqual, Null, Null) => Boolean(false),
            (ComparationOp::StrictNotEqual, _, Null) => Boolean(true),
            (ComparationOp::StrictNotEqual, Null, _) => Boolean(true),

            (ComparationOp::LessThan, Number(a), Number(b)) => Boolean(a < b),
            // TODO ver como funciona a comparação de strings do TS
            (ComparationOp::LessThan, Str(a), Str(b)) => {
                Boolean(a.chars().count() < b.chars().count())
            }

            (ComparationOp::LessEqual, Number(a), Number(b)) => Boolean(a <= b),
            // TODO ver como funciona a comparação de strings do TS
            (ComparationOp::LessEqual, Str(a), Str(b)) => {
                Boolean(a.chars().count() <= b.chars().count())
            }

            (ComparationOp::GreaterThan, Number(a), Number(b)) => Boolean(a > b),
            // TODO ver como funciona a comparação de strings do TS
            (ComparationOp::GreaterThan, Str(a), Str(b)) => {
                Boolean(a.chars().count() > b.chars().count())
            }

            (ComparationOp::GreaterEqual, Number(a), Number(b)) => Boolean(a >= b),
            // TODO ver como funciona a comparação de strings do TS
            (ComparationOp::GreaterEqual, Str(a), Str(b)) => {
                Boolean(a.chars().count() >= b.chars().count())
            }

            _ => panic!("at the disco"),
        };
        Ok(result)
    }

    fn eval_logical_expr(
        &mut self,
        left: &Expr,
        op_and_token: &OpWithToken<LogicalOp>,
        right: &Expr,
    ) -> InterpreterResult {
        use Value::*;
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        let (op, _) = op_and_token;

        let result = match (op, eval_left, eval_right) {
            (LogicalOp::And, Boolean(a), Boolean(b)) => Boolean(a && b),
            (LogicalOp::Or, Boolean(a), Boolean(b)) => Boolean(a || b),
            _ => panic!("LogicalOp error"),
        };
        Ok(result)
    }

    fn eval_var_expr(&self, identifier: &str, token: &Token) -> InterpreterResult {
        if let Some(value) = self.environment.get(identifier) {
            Ok(value)
        } else {
            Err(RuntimeError::VariableNotDeclared(
                token.line(),
                token.starts_at(),
                token.ends_at(),
            ))
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> InterpreterResult {
        match expr {
            Expr::Arithmetic(left, op_and_token, right) => {
                self.eval_binary_arith_expr(left, op_and_token, right)
            }
            Expr::Comparation(left, op_and_token, right) => {
                self.eval_comp_expr(left, op_and_token, right)
            }
            Expr::Logical(left, op_and_token, right) => {
                self.eval_logical_expr(left, op_and_token, right)
            }
            Expr::Unary(op_and_token, right) => self.eval_unary_expr(op_and_token, right),
            Expr::Grouping(new_expr) => self.eval_expr(new_expr),
            Expr::Variable(token, identifier) => self.eval_var_expr(identifier, token),
            Expr::Literal(value_and_token) => Ok(value_and_token.clone().0),
            Expr::Call(callee, params) => self.eval_call(expr, callee, params),
            Expr::Get(expr, token) => self.eval_get(expr, token),
            Expr::Instantiate(type_token, args) => self.eval_instantiate(type_token, args),
        }
    }

    fn eval_get(&mut self, expr: &Expr, token: &Token) -> InterpreterResult {
        let object = self.eval_expr(expr)?;
        match object {
            Value::TypeInstance(mint_instance) => Ok(mint_instance.get(&token).unwrap().clone()),
            _ => panic!("cannot get instance"), // TODO error
        }
    }

    fn eval_instantiate(
        &mut self,
        type_token: &Token,
        args: &[(Token, Expr)],
    ) -> InterpreterResult {
        if let Some(Value::Type(mint_type)) = self.environment.get(&type_token.lexeme()) {
            let mut values = vec![];
            for (arg, expr) in args {
                values.push((arg.clone(), self.eval_expr(expr)?));
            }
            Ok(Value::TypeInstance(mint_type.call(&values)))
        } else {
            Err(RuntimeError::NotInstantiable(
                type_token.line(),
                type_token.starts_at(),
                type_token.ends_at(),
                type_token.lexeme(),
            ))
        }
    }

    fn eval_call(
        &mut self,
        expr: &Expr,
        callee: &Expr,
        params: &[Expr],
    ) -> Result<Value, RuntimeError> {
        if let Value::Fun(fun) = self.eval_expr(callee)? {
            if fun.arity() == params.len() {
                let mut args = Vec::with_capacity(params.len());
                for expr in params {
                    args.push(self.eval_expr(expr)?);
                }
                fun.call(self, args.as_slice())
            } else {
                let (line, starts_at, ends_at) = expr.placement();
                Err(RuntimeError::ArityMismatch(
                    line,
                    starts_at,
                    ends_at,
                    fun.arity(),
                    params.len(),
                ))
            }
        } else {
            let (line, starts_at, ends_at) = callee.placement();
            Err(RuntimeError::NotCallable(line, starts_at, ends_at))
        }
    }

    pub fn eval_func(&mut self, fun: &Callable, args: &[Value]) -> Result<Value, RuntimeError> {
        let old = std::mem::replace(&mut self.environment, fun.env().clone());
        let result = self.with_new_env(|interpreter| {
            for ((param, _), value) in fun.params().iter().zip(args) {
                interpreter
                    .environment
                    .define(param.lexeme(), value.clone());
            }

            for stmt in fun.body() {
                match interpreter.eval(&stmt) {
                    Err(RuntimeError::Return(value)) => return Ok(value),
                    Err(err) => return Err(err),
                    Ok(()) => {}
                }
            }
            Ok(Value::Null)
        });
        self.environment = old;
        result
    }

    fn eval_var_stmt(&mut self, identifier: &str, expr: &Expr) -> Result<(), RuntimeError> {
        let value = self.eval_expr(expr)?;
        self.environment.define(identifier.to_string(), value);
        Ok(())
    }

    fn with_new_env<T>(&mut self, fun: impl Fn(&mut Self) -> T) -> T {
        self.environment.push();
        let result = fun(self);
        self.environment.pop();
        result
    }

    fn eval(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    println!("{}", value);
                    Ok(())
                }
                Err(err) => Err(err),
            },
            Stmt::AssertStmt(expr) => match self.eval_expr(expr) {
                Ok(Value::Boolean(true)) => Ok(()),
                Ok(_) => {
                    let (line, starts_at, ends_at) = expr.placement();
                    Err(RuntimeError::AssertionFailed(line, starts_at, ends_at))
                }
                Err(err) => Err(err),
            },
            Stmt::VarStmt(identifier, _, expr) => match self.eval_var_stmt(identifier, expr) {
                Ok(_) => Ok(()),
                Err(err) => Err(err),
            },
            Stmt::Block(stmts) => self.with_new_env(|interpreter| {
                for stmt in stmts {
                    let res = interpreter.eval(stmt);
                    if res.is_err() {
                        return res;
                    }
                }
                Ok(())
            }),
            Stmt::IfStmt(cond, then, else_) => match self.eval_expr(cond) {
                Ok(val) => match val.to_bool() {
                    true => {
                        for then in then {
                            self.eval(then)?;
                        }
                        Ok(())
                    }
                    false => {
                        for else_ in else_ {
                            self.eval(else_)?;
                        }
                        Ok(())
                    }
                },
                Err(err) => Err(err),
            },
            Stmt::Function(token, params, body, return_type, _) => {
                self.environment.define(
                    token.lexeme(),
                    Value::new_function(
                        self.environment.clone(),
                        token.clone(),
                        params.clone(),
                        body.clone(),
                        return_type.clone(),
                    ),
                );
                Ok(())
            }
            Stmt::Return(_, expr) => Err(RuntimeError::Return(if let Some(expr) = expr {
                self.eval_expr(expr)?
            } else {
                Value::Null
            })),
            Stmt::TypeStmt(token, args) => {
                self.environment.define(
                    token.lexeme(),
                    Value::Type(MintType::new(token.clone(), args)),
                );

                Ok(())
            }
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Option<Error> {
        let global = HashMap::new();
        self.environment = Environment::new(global);
        for stmt in stmts {
            if let Err(error) = self.eval(stmt) {
                return Some(Error::Runtime(error));
            }
        }
        None
    }
}
