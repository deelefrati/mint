use crate::{
    error::{parser::ParserError, Error},
    expr::*,
    stmt::Stmt,
    token::Token,
    token_type::{Literal, TokenType, VarType},
};

use std::collections::HashMap;

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
        } else if self.next_is(single(Type)).is_some() {
            self.type_declaration()
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
            if let Some((var_type, _)) = self.next_is_type() {
                params.push((param_id, var_type));
                while self.next_is(single(Comma)).is_some() {
                    let param_id = self.consume(Identifier)?;
                    self.consume(Colon)?;
                    if let Some((var_type, _)) = self.next_is_type() {
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
        let mut return_token = None;
        if self.next_is(single(Colon)).is_some() {
            if let Some((var_type, token)) = self.next_is_type() {
                return_type = var_type;
                return_token = Some(token);
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
            return_token,
        ))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(LeftParen)?;
        let cond = self.expression(None)?;
        self.consume(RightParen)?;
        let then = self.block()?;
        let else_ = if self.next_is(single(Else)).is_some() {
            self.block()?
        } else {
            vec![]
        };
        Ok(Stmt::IfStmt(cond, then, else_))
    }

    fn type_declaration(&mut self) -> Result<Stmt, ParserError> {
        let identifier = self.consume(Identifier)?;
        self.consume(Equal)?;
        if self.next_is(single(LeftBrace)).is_some() {
            let mut variables = HashMap::default();
            while self.next_is(single(RightBrace)).is_none() {
                let id = self.consume(Identifier)?;
                self.consume(Colon)?;
                let (var_type, _) = self.consume_type()?;
                variables.insert(id.lexeme(), var_type);
                self.consume(Comma)?;
            }
            self.consume(Semicolon)?;
            if variables.is_empty() {
                return Err(ParserError::EmptyType);
            }
            Ok(Stmt::TypeStmt(identifier, variables))
        } else {
            let type_ = self.consume_type()?;
            self.consume(Semicolon)?;
            Ok(Stmt::TypeAlias(identifier, type_))
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let identifier = self.consume(Identifier)?;
        if self.next_is(single(Colon)).is_some() {
            let (var_type, id) = self.consume_type()?;
            self.consume(Equal)?;
            let expr = self.expression(Some((id, var_type.clone())))?;
            self.consume(Semicolon)?;
            Ok(Stmt::VarStmt(identifier.lexeme(), Some(var_type), expr))
        } else if self.next_is(single(Equal)).is_some() {
            let expr = self.expression(None)?;
            self.consume(Semicolon)?;
            Ok(Stmt::VarStmt(identifier.lexeme(), None, expr))
        } else {
            Err(ParserError::Expected(self.current_line, vec![Equal]))
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if let Ok(ret_token) = self.consume(Return) {
            self.return_(&ret_token)
        } else if self.consume(Console).is_ok() {
            self.console()
        } else {
            self.expression_statement()
        }
    }

    fn return_(&mut self, token: &Token) -> Result<Stmt, ParserError> {
        Ok(Stmt::Return(
            token.clone(),
            if self.next_is(single(Semicolon)).is_none() {
                let expr = self.expression(None)?;
                self.consume(Semicolon)?;
                Some(expr)
            } else {
                None
            },
        ))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        self.consume(LeftBrace)?;
        Ok(self.list_statements()?)
    }

    fn list_statements(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            if self.next_is(single(RightBrace)).is_some() {
                return Ok(stmts);
            }
            stmts.push(self.declaration()?);
        }
        Err(ParserError::Expected(self.current_line, vec![RightBrace]))
    }

    fn console(&mut self) -> Result<Stmt, ParserError> {
        self.consume(Dot)?;
        if self.next_is(single(Log)).is_some() {
            self.consume(LeftParen)?;
            let mut exprs: Vec<Expr> = vec![];
            while self.next_is(single(RightParen)).is_none() {
                exprs.push(self.expression(None)?);
                self.next_is(single(Comma));
            }
            self.consume(Semicolon)?;
            Ok(Stmt::PrintStmt(exprs))
        } else if self.next_is(single(Assert)).is_some() {
            self.consume(LeftParen)?;
            let expr = self.expression(None)?;
            self.consume(RightParen)?;
            self.consume(Semicolon)?;
            Ok(Stmt::AssertStmt(expr))
        } else {
            Err(ParserError::Expected(self.current_line, vec![Log, Assert]))
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.expression(None) {
            Ok(value) => {
                self.consume(Semicolon)?;
                Ok(Stmt::ExprStmt(value))
            }
            Err(error) => Err(error),
        }
    }

    fn expression(&mut self, t: Option<(Token, VarType)>) -> Result<Expr, ParserError> {
        if self.next_is(single(LeftBrace)).is_some() {
            self.object(t.unwrap())
        } else {
            self.or()
        }
    }

    fn object(&mut self, t: (Token, VarType)) -> Result<Expr, ParserError> {
        let (token, var_type) = t;
        let mut variables = vec![];
        while self.next_is(single(RightBrace)).is_none() {
            let identifier = self.consume(Identifier)?;
            self.consume(Colon)?;
            let expr = self.expression(None)?;
            self.consume(Comma)?;
            variables.push((identifier, expr));
        }
        Ok(Expr::Instantiate(token, variables, var_type))
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
        loop {
            if self.next_is(single(LeftParen)).is_some() {
                expr = self.finish_call(expr)?;
            } else if self.next_is(single(Dot)).is_some() {
                let identifier = self.consume(Identifier)?;
                expr = Expr::Get(Box::new(expr), identifier);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments: Vec<Expr> = Vec::new();
        if self.next_is(single(RightParen)).is_none() {
            arguments.push(self.expression(None)?);
            while self.next_is(single(Comma)).is_some() {
                arguments.push(self.expression(None)?);
            }

            self.consume(RightParen)?;
        }
        Ok(Expr::Call(Box::new(callee), arguments))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.next_is(single(LeftParen)).is_some() {
            let expr = self.expression(None)?;
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
        } else if self.next_is(single(Typeof)).is_some() {
            let left = self.call()?;
            self.consume(EqualEqualEqual)?;
            let literal = self.consume_literal_string()?;
            Ok(Expr::Typeof(left.into(), literal))
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
        Err(ParserError::Missing(self.current_line, tt))
    }

    fn consume_literal_string(&mut self) -> Result<(VarType, Token), ParserError> {
        if let Some((var_type, t)) = self.next_is(|tt| match tt {
            TokenType::String(string) => match string.as_str() {
                "string" => Some(VarType::String),
                "number" => Some(VarType::Number),
                "boolean" => Some(VarType::Boolean),
                "null" => Some(VarType::Null),
                "object" => Some(VarType::UserType(Token::default())),
                _ => None,
            },
            _ => None,
        }) {
            if matches!(var_type, VarType::UserType(_)) {
                Ok((VarType::UserType(t.clone()), t))
            } else {
                Ok((var_type, t))
            }
        } else {
            Err(ParserError::Missing(
                self.current_line,
                String("construir outro erro".to_string()),
            )) // TODO construir outro erro
        }
    }

    fn consume_type(&mut self) -> Result<(VarType, Token), ParserError> {
        if let Some((var_type, token)) = self.next_is_type() {
            Ok((var_type, token))
        } else {
            Err(ParserError::TypeExpected(self.current_line))
        }
    }

    fn next_is_type(&mut self) -> Option<(VarType, Token)> {
        if let Some((t, token)) = self.is_type() {
            if self.next_is(single(Pipe)).is_some() {
                if let Some(union) = self.union((t, token.clone())) {
                    return Some((VarType::Union(union), token));
                } else {
                    return None;
                }
            }
            return Some((t, token));
        }

        None
    }

    fn is_type(&mut self) -> Option<(VarType, Token)> {
        if let Some(token) = self.tokens.first() {
            if let Some(tt) = match token.token_type() {
                TokenType::Null => Some(VarType::Null),
                TokenType::Num => Some(VarType::Number),
                TokenType::Str => Some(VarType::String),
                TokenType::Bool => Some(VarType::Boolean),
                TokenType::Identifier => Some(VarType::UserType(token.clone())),
                TokenType::String(string) => {
                    Some(VarType::Literals(Literal::String(string.to_string())))
                }
                TokenType::Number(number) => Some(VarType::Literals(Literal::Number(*number))),
                TokenType::True => Some(VarType::Literals(Literal::Boolean(true))),
                TokenType::False => Some(VarType::Literals(Literal::Boolean(false))),
                _ => None,
            } {
                self.advance();
                Some((tt, token.clone()))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn union(&mut self, first: (VarType, Token)) -> Option<Vec<(VarType, Token)>> {
        let mut types = vec![first];
        while let Some(type_) = self.is_type() {
            types.push(type_);
            if self.next_is(single(Pipe)).is_none() {
                return Some(types);
            }
        }
        if types.is_empty() {
            None
        } else {
            Some(types)
        }
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
