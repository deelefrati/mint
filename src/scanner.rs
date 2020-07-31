#![allow(dead_code)]
use crate::error::{Error, ScannerError};
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

    fn is_at_end(&self) -> bool {
        self.chars.clone().next().is_none()
    }

    fn scan_token(&mut self) -> Result<TokenType, Error> {
        use TokenType::*;
        let c = self.advance();
        match c {
            Some('(') => Ok(LeftParen),
            Some(')') => Ok(RightParen),
            Some('{') => Ok(LeftBrace),
            Some('}') => Ok(RightBrace),
            Some(',') => Ok(Comma),
            Some('.') => Ok(Dot),
            Some('-') => Ok(Minus),
            Some('+') => Ok(Plus),
            Some('*') => Ok(Star),
            Some('!') => match self.match_char('=') {
                Ok(true) => match self.match_char('=') {
                    Ok(true) => Ok(BangEqualEqual),

                    Ok(false) => Ok(BangEqual),
                    Err(error) => Err(error),
                },
                Ok(false) => Ok(Bang),
                Err(error) => Err(error),
            },
            Some('=') => match self.match_char('=') {
                Ok(true) => match self.match_char('=') {
                    Ok(true) => Ok(EqualEqualEqual),

                    Ok(false) => Ok(EqualEqual),
                    Err(error) => Err(error),
                },
                Ok(false) => Ok(Equal),
                Err(error) => Err(error),
            },
            Some('<') => match self.match_char('=') {
                Ok(true) => Ok(LessEqual),
                Ok(false) => Ok(Less),
                Err(error) => Err(error),
            },
            Some('>') => match self.match_char('=') {
                Ok(true) => Ok(GreaterEqual),
                Ok(false) => Ok(Greater),
                Err(error) => Err(error),
            },
            Some('&') => match self.match_char('&') {
                Ok(true) => Ok(And),
                Ok(false) => {
                    Err(Error::Scanner(ScannerError::InvalidToken(
                        self.line,
                        std::string::String::from("Did you mean  \"&&\""), //TODO formartar erro corretamente
                        self.start_token,
                        self.end_token,
                    )))
                }
                Err(error) => Err(error),
            },
            Some('|') => match self.match_char('|') {
                Ok(true) => Ok(Or),
                Ok(false) => {
                    Err(Error::Scanner(ScannerError::InvalidToken(
                        self.line,
                        std::string::String::from("Did you mean  \"||\""), //TODO formartar erro corretamente
                        self.start_token,
                        self.end_token,
                    )))
                }
                Err(error) => Err(error),
            },
            Some('/') => self.slash_or_comment(),
            Some(' ') | Some('\r') | Some('\t') => Ok(Blank),
            Some('\n') => Ok(self.newline()),
            Some('"') => self.string(),
            Some(_) => {
                if is_digit(c.unwrap()) {
                    self.number()
                } else if is_alpha(c.unwrap()) {
                    self.identifier()
                } else {
                    Err(Error::Scanner(ScannerError::InvalidToken(
                        self.line,
                        "Invalid character".to_string(),
                        self.start_token,
                        self.end_token,
                    )))
                }
            }
            None => panic!("Unexpected error at scan token"),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Error> {
        use TokenType::*;
        while !self.is_at_end() {
            self.start_token = self.end_token;
            match self.scan_token() {
                Ok(token) => match token {
                    NewLine => {
                        self.start_token = 0;
                        self.end_token = 0;
                    }
                    Blank | Comment => (),
                    _ => self.add_token(token),
                },
                Err(scanner_error) => return Err(scanner_error),
            }
            self.source = self.chars.as_str();
        }
        self.add_token(Eof);
        Ok(&self.tokens)
    }

    fn add_token(&mut self, tt: TokenType) {
        self.tokens
            .push(Token::new(tt, self.line, self.start_token, self.end_token));
    }

    fn advance(&mut self) -> Option<char> {
        self.end_token += 1;
        self.chars.next()
    }

    fn match_char(&mut self, expected: char) -> Result<bool, Error> {
        if let Some(c) = self.peek() {
            if self.is_at_end() {
                Ok(false)
            } else if c != expected {
                Ok(false)
            } else {
                self.advance();
                Ok(true)
            }
        } else {
            Err(Error::Unexpected)
        }
    }

    fn slash_or_comment(&mut self) -> Result<TokenType, Error> {
        match self.match_char('/') {
            Ok(true) => {
                self.advance();
                while self.peek() != Some('\n') && !self.is_at_end() {
                    self.advance();
                }
                Ok(TokenType::Comment)
            }
            Ok(false) => Ok(TokenType::Slash),
            Err(error) => Err(error),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn newline(&mut self) -> TokenType {
        self.line += 1;
        TokenType::NewLine
    }

    fn string(&mut self) -> Result<TokenType, Error> {
        let starting_line = self.line;
        self.take_while(|ch| ch != '"');
        if self.peek() == Some('"') {
            let string = TokenType::String(self.consumed()[1..].into());
            self.advance();
            Ok(string)
        } else {
            Err(Error::Scanner(ScannerError::UnterminatedString(
                starting_line,
            )))
        }
    }

    fn source_substring(&self, start: usize, finish: usize) -> &str {
        &self.source[start..finish]
    }

    fn number(&mut self) -> Result<TokenType, Error> {
        self.take_while(is_digit);
        if Some('.') == self.peek() {
            self.advance();
            if is_digit(self.peek().unwrap_or(' ')) {
                self.take_while(is_digit);
                if Some('.') == self.peek() {
                    self.advance();
                    self.take_while(is_digit);
                    return Err(Error::Scanner(ScannerError::InvalidToken(
                        self.line,
                        format!("Failed parsing number {}", &self.consumed()),
                        self.start_token,
                        self.end_token,
                    )));
                } else {
                    Ok(TokenType::Number(self.consumed().parse::<f64>().unwrap()))
                }
            } else {
                Err(Error::Scanner(ScannerError::InvalidToken(
                    self.line,
                    format!("Failed parsing number {}", &self.consumed()),
                    self.start_token,
                    self.end_token,
                )))
            }
        } else {
            Ok(TokenType::Number(self.consumed().parse().unwrap()))
        }
    }

    fn identifier(&mut self) -> Result<TokenType, Error> {
        self.take_while(is_alphanumeric);

        // Check if the text is a reserved word
        let text = self.consumed();
        if let Some(tt) = self.is_keyword(&text) {
            Ok(tt)
        } else {
            Ok(TokenType::Identifier(String::from(text)))
        }
    }

    fn is_keyword(&self, token: &str) -> Option<TokenType> {
        use TokenType::*;
        match token {
            "class" => Some(Class),
            "else" => Some(Else),
            "false" => Some(False),
            "function" => Some(Function),
            "if" => Some(If),
            "null" => Some(Null),
            "console.log" => Some(Print),
            "return" => Some(Return),
            "this" => Some(This),
            "true" => Some(True),
            "const" => Some(Const),
            _ => None,
        }
    }
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}
