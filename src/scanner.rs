use crate::error::{scanner::ScannerError, Error};
use crate::token::Token;
use crate::token_type::TokenType;
use std::str::Chars;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    source: &'a str,
    chars: Chars<'a>,
    tokens: Vec<Token>,
    start_token: usize,
    end_token: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: vec![],
            chars: source.chars(),
            start_token: 0,
            end_token: 0,
            line: 1,
        }
    }

    fn take_while(&mut self, fun: impl Fn(char) -> bool) {
        while self.peek().map(&fun).unwrap_or(false) {
            self.advance();
        }
    }

    fn consumed(&self) -> &'a str {
        let mut iter = self.source.chars();
        let mut len = 0;
        while !std::ptr::eq(iter.as_str(), self.chars.as_str()) {
            len += iter.next().map(char::len_utf8).unwrap_or(0);
        }
        &self.source[..len]
    }

    fn scan_token(&mut self) -> Result<Option<TokenType>, Error> {
        use TokenType::*;
        if let Some(c) = self.advance() {
            let token = match c {
                '(' => LeftParen,
                ')' => RightParen,
                '{' => LeftBrace,
                '}' => RightBrace,
                ',' => Comma,
                '.' => Dot,
                '-' => {
                    if is_digit(self.peek().unwrap_or_default()) {
                        self.parse_digit()?
                    } else {
                        Minus
                    }
                }

                '+' => Plus,
                '*' => Star,
                '%' => Mod,
                ':' => Colon,
                ';' => Semicolon,
                '!' => self.match_double_char_or_else('=', BangEqualEqual, Bang)?,
                '=' => self.match_double_char_or_else('=', EqualEqualEqual, Equal)?,
                '<' => self.match_char_or_else('=', LessEqual, Less),
                '>' => self.match_char_or_else('=', GreaterEqual, Greater),
                '&' => {
                    if self.match_char('&') {
                        And
                    } else {
                        return Err(Error::Scanner(ScannerError::InvalidToken(
                            self.line,
                            TokenType::And, //TODO formartar erro corretamente
                            self.start_token,
                            self.end_token,
                        )));
                    }
                }
                '\n' => self.newline(),
                '|' => self.match_char_or_else('|', Or, Pipe),
                '/' => self.slash_or_comment(),
                ' ' | '\r' | '\t' => Blank,
                '"' => {
                    if let Some(string) = self.string() {
                        string
                    } else {
                        return Err(Error::Scanner(ScannerError::UnterminatedString(self.line)));
                    }
                }
                c => {
                    if is_digit(c) {
                        self.parse_digit()?
                    } else if is_alpha(c) {
                        self.identifier_or_keyword()
                    } else {
                        return Err(Error::Scanner(ScannerError::InvalidString(
                            self.line,
                            self.start_token,
                            self.end_token,
                        )));
                    }
                }
            };
            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Error> {
        use TokenType::*;
        loop {
            self.start_token = self.end_token;
            if let Some(token) = self.scan_token()? {
                match token {
                    NewLine => {
                        self.start_token = 0;
                        self.end_token = 0;
                    }
                    Blank | Comment => (),
                    _ => self.add_token(token, self.consumed().to_string()),
                }
            } else {
                break;
            }
            self.source = self.chars.as_str();
        }
        self.add_token(Eof, "EOF".to_string());
        Ok(&self.tokens)
    }

    fn add_token(&mut self, tt: TokenType, lexeme: String) {
        self.tokens.push(Token::new(
            tt,
            self.line,
            self.start_token,
            self.end_token,
            lexeme,
        ));
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            self.end_token += 1;
            Some(ch)
        } else {
            None
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if Some(expected) == self.peek() {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_char_or_else(&mut self, expected: char, tt1: TokenType, tt2: TokenType) -> TokenType {
        if Some(expected) == self.peek() {
            self.advance();
            tt1
        } else {
            tt2
        }
    }

    fn match_double_char_or_else(
        &mut self,
        expected: char,
        tt1: TokenType,
        tt2: TokenType,
    ) -> Result<TokenType, Error> {
        if Some(expected) == self.peek() {
            self.advance();
            if Some(expected) == self.peek() {
                self.advance();
                Ok(tt1)
            } else {
                Err(Error::Scanner(ScannerError::InvalidToken(
                    self.line,
                    tt1,
                    self.start_token,
                    self.end_token,
                )))
            }
        } else {
            Ok(tt2)
        }
    }

    fn slash_or_comment(&mut self) -> TokenType {
        if self.match_char('/') {
            self.take_while(|ch| ch != '\n');
            TokenType::Comment
        } else {
            TokenType::Slash
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn newline(&mut self) -> TokenType {
        self.line += 1;
        TokenType::NewLine
    }

    fn string(&mut self) -> Option<TokenType> {
        self.take_while(|ch| ch != '"');
        if self.peek() == Some('"') {
            let string = TokenType::String(self.consumed()[1..].into());
            self.advance();
            Some(string)
        } else {
            None
        }
    }

    fn number(&mut self) -> Option<TokenType> {
        self.take_while(is_digit);
        if Some('.') == self.peek() {
            self.advance();
            if is_digit(self.peek().unwrap_or(' ')) {
                self.take_while(is_digit);
                if Some('.') == self.peek() {
                    self.advance();
                    self.take_while(is_digit);
                    None
                } else {
                    Some(TokenType::Number(self.consumed().parse::<f64>().unwrap()))
                }
            } else {
                None
            }
        } else {
            Some(TokenType::Number(self.consumed().parse().unwrap()))
        }
    }

    fn identifier_or_keyword(&mut self) -> TokenType {
        self.take_while(is_alphanumeric);
        let text = self.consumed();
        self.get_keyword(&text).unwrap_or(TokenType::Identifier)
    }

    fn get_keyword(&self, token: &str) -> Option<TokenType> {
        use TokenType::*;
        match token {
            "assert" => Some(Assert),
            "type" => Some(Type),
            "else" => Some(Else),
            "false" => Some(False),
            "function" => Some(Function),
            "if" => Some(If),
            "null" => Some(Null),
            "log" => Some(Log),
            "console" => Some(Console),
            "return" => Some(Return),
            "true" => Some(True),
            "const" => Some(Const),
            "number" => Some(Num),
            "string" => Some(Str),
            "boolean" => Some(Bool),
            "typeof" => Some(Typeof),
            "import" => Some(Import),
            "from" => Some(From),
            _ => None,
        }
    }

    fn parse_digit(&mut self) -> Result<TokenType, Error> {
        if let Some(number) = self.number() {
            Ok(number)
        } else {
            Err(Error::Scanner(ScannerError::InvalidNumber(
                self.line,
                TokenType::String(self.consumed().to_string()),
                self.start_token,
                self.end_token,
            )))
        }
    }
}

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

fn is_alpha(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}
