use crate::{
    error::{Error, RuntimeError},
    expr::*,
    stmt::*,
};

type InterpreterResult = std::result::Result<Value, RuntimeError>;

#[derive(Default)]
pub struct Interpreter {}

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
            (Bang, Value::Boolean(a)) => Ok(Boolean(!*a)),
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

    fn eval_expr(&self, expr: &Expr) -> InterpreterResult {
        use Expr::*;
        match expr {
            Arithmetic(left, op_and_token, right) => {
                self.eval_binary_arith_expr(left, op_and_token, right)
            }
            Unary(op_and_token, right) => self.eval_unary_expr(op_and_token, right),
            Grouping(new_expr) => self.eval_expr(new_expr),
            Literal(value_and_token) => Ok(value_and_token.clone().0),
            _ => unimplemented!(),
        }
    }

    fn eval(&self, stmt: &Stmt) -> Option<Error> {
        use Stmt::*;
        match stmt {
            ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    println!("{}", value);
                    None
                }
                Err(error) => Some(Error::Runtime(error)),
            },
            c => {
                println!("{:?}", c);
                None
            }
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Option<Error> {
        for stmt in stmts {
            if let Some(evaluation) = self.eval(stmt) {
                return Some(evaluation);
            }
        }
        None
    }
}
