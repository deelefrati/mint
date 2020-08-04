use crate::{
    error::{Error, ParserError},
    expr::*,
    stmt::Stmt,
    token::Token,
    token_type::TokenType,
};
use std::vec::Vec;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

use TokenType::*;
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut statements: Vec<Stmt> = vec![];
        let mut errors: Vec<Error> = vec![];

        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(Error::Parser(error)),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn is_at_end(&self) -> bool {
        if let Some(token) = self.peek() {
            *token.token_type() == Eof
        } else {
            true
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.first()
    }

    fn advance(&mut self) -> Option<&Token> {
        self.tokens = &self.tokens[1..]; //FIXME
        Some(&self.tokens[0])
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        self.expression_statement()
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.expression() {
            Ok(value) => Ok(Stmt::ExprStmt(value)),
            Err(error) => Err(error),
        }
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            EqualEqual => Some(BinaryOp::Equal),
            EqualEqualEqual => Some(BinaryOp::TypedEqual),
            _ => None,
        }) {
            let right = self.comparison()?;
            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.addition()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Greater => Some(BinaryOp::GreaterThan),
            GreaterEqual => Some(BinaryOp::GreaterEqual),
            Less => Some(BinaryOp::LessThan),
            LessEqual => Some(BinaryOp::LessEqual),
            _ => None,
        }) {
            let right = self.addition()?;
            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.multiplication()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Minus => Some(BinaryOp::Sub),
            Plus => Some(BinaryOp::Add),
            _ => None,
        }) {
            let right = self.multiplication()?;
            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Slash => Some(BinaryOp::Div),
            Star => Some(BinaryOp::Mul),
            Mod => Some(BinaryOp::Mod),
            _ => None,
        }) {
            let right = self.unary()?;
            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(op_and_token) = self.next_is(|tt| match tt {
            Minus => Some(UnaryOp::Minus),
            Plus => Some(UnaryOp::Plus),
            Bang => Some(UnaryOp::Bang),
            _ => None,
        }) {
            Ok(Expr::Unary(op_and_token, self.unary()?.into()))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(_) = self.next_is(|tt| match tt {
            LeftParen => Some(LeftParen),
            _ => None,
        }) {
            let expr = self.expression()?;
            match self.consume(
                RightParen,
                "Expected a ')' after this expression".to_string(),
            ) {
                Ok(_) => Ok(Expr::Grouping(expr.into())),
                Err(error) => Err(error),
            }
        } else if let Some(op_and_token) = self.next_is(|tt| match tt {
            Number(num) => Some(Value::Number(*num)),
            String(str) => Some(Value::Str(str.to_string())),
            True => Some(Value::Boolean(true)),
            False => Some(Value::Boolean(false)),
            Null => Some(Value::Null),
            _ => None,
        }) {
            Ok(Expr::Literal(op_and_token))
        } else {
            Err(ParserError::MissingExpression(self.peek().unwrap().line())) //FIXME retirar o unwrap
        }
    }

    fn next_is<T>(&mut self, fun: impl Fn(&TokenType) -> Option<T>) -> Option<(T, Token)> {
        if let Some(token) = self.tokens.get(self.current) {
            if let Some(t) = fun(token.token_type()) {
                self.advance();
                return Some((t, token.clone()));
            }
        }
        None
    }

    fn check_type(&self, tt: TokenType) -> bool {
        if let Some(token) = self.peek() {
            *token.token_type() == tt
        } else {
            false
        }
    }

    fn consume(
        &mut self,
        tt: TokenType,
        error_message: std::string::String,
    ) -> Result<Option<&Token>, ParserError> {
        if self.check_type(tt) {
            Ok(self.advance())
        } else {
            if let Some(token) = self.peek() {
                Err(ParserError::Missing(
                    token.line(),
                    error_message.to_string(),
                ))
            } else {
                Ok(None) //FIXME esse caso nunca acontece
            }
        }
    }
}
