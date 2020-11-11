use crate::{
    environment::Environment,
    error::{Error, RuntimeError},
    expr::*,
    stmt::*,
};

type InterpreterResult = std::result::Result<Value, RuntimeError>;

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

        // FIXME panic will never happen
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
            Ok(value.clone())
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
        }
    }

    fn eval_var_stmt(&mut self, identifier: &str, expr: &Expr) -> Result<(), RuntimeError> {
        let value = self.eval_expr(expr)?;

        self.environment.define(identifier.to_string(), value);
        Ok(())
    }

    fn eval(&mut self, stmt: &Stmt) -> Option<Error> {
        use Stmt::*;
        match stmt {
            ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    println!("{}", value);
                    None
                }
                Err(error) => Some(Error::Runtime(error)),
            },
            AssertStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    if value != Value::Boolean(true) {
                        Some(Error::Runtime(RuntimeError::AssertionFailed))
                    } else {
                        None
                    }
                }
                Err(error) => Some(Error::Runtime(error)),
            },
            VarStmt(identifier, _, expr) => match self.eval_var_stmt(identifier, expr) {
                Ok(()) => None,
                Err(error) => Some(Error::Runtime(error)),
            },
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Option<Error> {
        self.environment = Environment::default();
        for stmt in stmts {
            if let Some(evaluation) = self.eval(stmt) {
                return Some(evaluation);
            }
        }
        // println!("{:?}", self.environment);
        None
    }
}
