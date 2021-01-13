use crate::{
    error::{Error, ParserError},
    expr::*,
    stmt::Stmt,
    token::Token,
    token_type::{TokenType, VarType},
};
use std::vec::Vec;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current_line: usize,
}

use TokenType::*;
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current_line: 1,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut statements: Vec<Stmt> = vec![];
        let mut errors: Vec<Error> = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    errors.push(Error::Parser(error));
                    self.syncronyze();
                }
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn syncronyze(&mut self) {
        while let Some(token) = self.peek() {
            match token.token_type() {
                Semicolon => {
                    self.advance();
                    return;
                }
                Type => return,
                Function => return,
                Const => return,
                If => return,
                Print => return,
                Return => return,
                _ => {
                    self.advance();
                }
            }
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
        if let Some((first, rest)) = self.tokens.split_first() {
            self.tokens = rest;
            self.current_line = first.line();
            Some(first)
        } else {
            None
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.next_is(single(Const)).is_some() {
            self.var_declaration()
        } else if self.next_is(single(If)).is_some() {
            self.if_statement()
        } else if self.next_is(single(Function)).is_some() {
            self.fun()
        } else {
            self.statement()
        }
    }

    fn fun(&mut self) -> Result<Stmt, ParserError> {
        let identifier = self.consume(Identifier)?;
        let mut params = vec![];
        self.consume(LeftParen)?;
        if self.next_is(single(RightParen)).is_none() {
            let param_id = self.consume(Identifier)?;
            self.consume(Colon)?;
            if let Some((var_type, _)) = self.next_is(|tt| match tt {
                Num => Some(VarType::Number),
                Str => Some(VarType::String),
                Bool => Some(VarType::Boolean),
                _ => None,
            }) {
                params.push((param_id, var_type));
                while self.next_is(single(Comma)).is_some() {
                    let param_id = self.consume(Identifier)?;
                    self.consume(Colon)?;
                    if let Some((var_type, _)) = self.next_is(|tt| match tt {
                        Num => Some(VarType::Number),
                        Str => Some(VarType::String),
                        Bool => Some(VarType::Boolean),
                        _ => None,
                    }) {
                        params.push((param_id, var_type));
                    } else {
                        return Err(ParserError::TypeNotDefined(self.current_line));
                    }
                }
            } else {
                return Err(ParserError::TypeNotDefined(self.current_line));
            }
            self.consume(RightParen)?;
        }
        let mut return_type = VarType::Null;
        if self.next_is(single(Colon)).is_some() {
            if let Some((var_type, _)) = self.next_is(|tt| match tt {
                Num => Some(VarType::Number),
                Str => Some(VarType::String),
                Bool => Some(VarType::Boolean),
                Null => Some(VarType::Null),
                _ => None,
            }) {
                return_type = var_type;
            } else {
                return Err(ParserError::ReturnTypeExpected(self.current_line));
            }
        }
        self.consume(LeftBrace)?;
        Ok(Stmt::Function(
            identifier,
            params,
            self.list_statements()?,
            return_type,
        ))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(LeftParen)?;
        let cond = self.expression()?;
        self.consume(RightParen)?;
        let then = vec![self.statement()?];
        let else_ = if self.next_is(single(Else)).is_some() {
            vec![self.statement()?]
        } else {
            vec![]
        };
        Ok(Stmt::IfStmt(cond, then, else_))
    }
    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let identifier = self.consume(Identifier)?;
        if self.next_is(single(Colon)).is_some() {
            if let Some((var_type, _)) = self.next_is(|tt| match tt {
                Num => Some(VarType::Number),
                Str => Some(VarType::String),
                Bool => Some(VarType::Boolean),
                Null => Some(VarType::Null),
                _ => None,
            }) {
                self.consume(Equal)?;
                let expr = self.expression()?;
                self.consume(Semicolon)?;
                Ok(Stmt::VarStmt(identifier.lexeme(), Some(var_type), expr))
            } else {
                Err(ParserError::TypeNotDefined(self.current_line))
            }
        } else if self.next_is(single(Equal)).is_some() {
            let expr = self.expression()?;
            self.consume(Semicolon)?;
            Ok(Stmt::VarStmt(identifier.lexeme(), None, expr))
        } else {
            Err(ParserError::AssignmentExpected(self.current_line))
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.consume(Assert).is_ok() {
            self.assert()
        } else if self.consume(LeftBrace).is_ok() {
            self.block()
        } else if self.consume(Return).is_ok() {
            self.return_()
        } else {
            self.expression_statement()
        }
    }

    fn return_(&mut self) -> Result<Stmt, ParserError> {
        Ok(Stmt::Return(if self.next_is(single(Semicolon)).is_none() {
            let expr = self.expression()?;
            self.consume(Semicolon)?;
            Some(expr)
        } else {
            None
        }))
    }

    fn block(&mut self) -> Result<Stmt, ParserError> {
        Ok(Stmt::Block(self.list_statements()?))
    }

    fn list_statements(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            if self.next_is(single(RightBrace)).is_some() {
                return Ok(stmts);
            }
            stmts.push(self.declaration()?);
        }
        Err(ParserError::Expected(self.current_line, RightBrace))
    }

    fn assert(&mut self) -> Result<Stmt, ParserError> {
        self.consume(LeftParen)?;
        let expr = self.expression()?;
        self.consume(RightParen)?;
        self.consume(Semicolon)?;
        Ok(Stmt::AssertStmt(expr))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.expression() {
            Ok(value) => Ok(Stmt::ExprStmt(value)),
            Err(error) => Err(error),
        }
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.or()
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Or => Some(LogicalOp::Or),
            _ => None,
        }) {
            let right = self.and()?;
            expr = Expr::Logical(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            And => Some(LogicalOp::And),
            _ => None,
        }) {
            let right = self.equality()?;
            expr = Expr::Logical(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            EqualEqual => Some(ComparationOp::Equal),
            EqualEqualEqual => Some(ComparationOp::StrictEqual),
            BangEqualEqual => Some(ComparationOp::StrictNotEqual),
            _ => None,
        }) {
            let right = self.comparison()?;
            expr = Expr::Comparation(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.addition()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Greater => Some(ComparationOp::GreaterThan),
            GreaterEqual => Some(ComparationOp::GreaterEqual),
            Less => Some(ComparationOp::LessThan),
            LessEqual => Some(ComparationOp::LessEqual),
            _ => None,
        }) {
            let right = self.addition()?;
            expr = Expr::Comparation(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.multiplication()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Minus => Some(ArithmeticOp::Sub),
            Plus => Some(ArithmeticOp::Add),
            _ => None,
        }) {
            let right = self.multiplication()?;
            expr = Expr::Arithmetic(expr.into(), op_and_token, right.into());
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Slash => Some(ArithmeticOp::Div),
            Star => Some(ArithmeticOp::Mul),
            Mod => Some(ArithmeticOp::Mod),
            _ => None,
        }) {
            let right = self.unary()?;
            expr = Expr::Arithmetic(expr.into(), op_and_token, right.into());
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
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        while self.next_is(single(LeftParen)).is_some() {
            expr = self.finish_call(expr)?;
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments: Vec<Expr> = Vec::new();
        if self.next_is(single(RightParen)).is_none() {
            arguments.push(self.expression()?);
            while self.next_is(single(Comma)).is_some() {
                arguments.push(self.expression()?);
            }

            self.consume(RightParen)?;
        }
        //self.consume(Semicolon)?;
        Ok(Expr::Call(Box::new(callee), arguments))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.next_is(single(LeftParen)).is_some() {
            let expr = self.expression()?;
            self.consume(RightParen)?;
            Ok(Expr::Grouping(expr.into()))
        } else if let Some(op_and_token) = self.next_is(|tt| match tt {
            Number(num) => Some(Value::Number(*num)),
            String(str) => Some(Value::Str(str.to_string())),
            True => Some(Value::Boolean(true)),
            False => Some(Value::Boolean(false)),
            Null => Some(Value::Null),
            _ => None,
        }) {
            Ok(Expr::Literal(op_and_token))
        } else if let Some((_, token)) = self.next_is(single(Identifier)) {
            Ok(Expr::Variable(token.clone(), token.lexeme()))
        } else {
            Err(ParserError::MissingExpression(self.current_line))
        }
    }

    fn next_is<T>(&mut self, fun: impl Fn(&TokenType) -> Option<T>) -> Option<(T, Token)> {
        if let Some(token) = self.tokens.first() {
            if let Some(t) = fun(token.token_type()) {
                self.advance();
                return Some((t, token.clone()));
            }
        }
        None
    }

    fn consume(&mut self, tt: TokenType) -> Result<Token, ParserError> {
        if let Some(token) = self.peek().cloned() {
            if *token.token_type() == tt {
                self.advance();
                return Ok(token);
            }
        }
        //println!("{:?}", self.tokens.first());
        Err(ParserError::Missing(self.current_line, tt))
    }
}
fn single(tt: TokenType) -> impl Fn(&TokenType) -> Option<()> {
    use std::mem::discriminant;
    move |other| {
        if discriminant(&tt) == discriminant(other) {
            Some(())
        } else {
            None
        }
    }
}
