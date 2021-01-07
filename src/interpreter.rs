use crate::{
    environment::Environment,
    error::{Error, RuntimeError},
    expr::*,
    stmt::*,
};
use std::collections::HashMap;

type InterpreterResult = Result<Value, RuntimeError>;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
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
                    println!("AAA");
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
        &self,
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
        &self,
        op_and_token: &OpWithToken<UnaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let eval_right = self.eval_expr(right)?;

        self.unary_op(op_and_token, &eval_right)
    }

    fn eval_comp_expr(
        &self,
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
        &self,
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

    fn eval_var_expr(&self, identifier: &str) -> InterpreterResult {
        if let Some(value) = self.environment.get(identifier) {
            Ok(value)
        } else {
            Err(RuntimeError::VariableNotDeclared)
        }
    }

    fn eval_expr(&self, expr: &Expr) -> InterpreterResult {
        use Expr::*;
        match expr {
            Arithmetic(left, op_and_token, right) => {
                self.eval_binary_arith_expr(left, op_and_token, right)
            }
            Comparation(left, op_and_token, right) => {
                self.eval_comp_expr(left, op_and_token, right)
            }
            Logical(left, op_and_token, right) => self.eval_logical_expr(left, op_and_token, right),
            Unary(op_and_token, right) => self.eval_unary_expr(op_and_token, right),
            Grouping(new_expr) => self.eval_expr(new_expr),
            Variable(_, identifier) => self.eval_var_expr(identifier),
            Literal(value_and_token) => Ok(value_and_token.clone().0),
            Call(calle, params) => if let Value::Fun(fun) = self.eval_expr(calee)? {},
        }
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

    fn eval(&mut self, stmt: &Stmt) -> Result<(), Error> {
        use Stmt::*;
        match stmt {
            ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(_) => Ok(()),
                Err(e) => Err(Error::Runtime(e)),
            },
            AssertStmt(expr) => match self.eval_expr(expr) {
                Ok(Value::Boolean(true)) => Ok(()),
                Ok(_) => Err(Error::Runtime(RuntimeError::AssertionFailed)),
                Err(error) => Err(Error::Runtime(error)),
            },
            VarStmt(identifier, _, expr) => match self.eval_var_stmt(identifier, expr) {
                Ok(_) => Ok(()),
                Err(error) => Err(Error::Runtime(error)),
            },
            Block(stmts) => self.with_new_env(|interpreter| {
                for stmt in stmts {
                    let res = interpreter.eval(stmt);
                    if res.is_err() {
                        return res;
                    }
                }
                Ok(())
            }),
            IfStmt(cond, then, r#else) => match self.eval_expr(cond) {
                Ok(val) => match val.to_bool() {
                    true => {
                        for then in then {
                            self.eval(then)?;
                        }
                        Ok(())
                    }
                    false => {
                        for r#else in r#else {
                            self.eval(r#else)?;
                        }
                        Ok(())
                    }
                },
                Err(e) => Err(Error::Runtime(e)),
            },
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Option<Error> {
        let global = HashMap::new();
        self.environment = Environment::new(global);
        for stmt in stmts {
            if let Err(error) = self.eval(stmt) {
                return Some(error);
            }
        }
        // println!("{:?}", self.environment);
        None
    }
}
