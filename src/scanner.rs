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
                '-' => Minus,
                '+' => Plus,
                '*' => Star,
                '%' => Mod,
                ':' => Colon,
                ';' => Semicolon,
                '!' => self.match_char_or_else_or_else('=', BangEqualEqual, BangEqual, Bang),
                '=' => self.match_char_or_else_or_else('=', EqualEqualEqual, EqualEqual, Equal),
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

                '|' => {
                    if self.match_char('|') {
                        Or
                    } else {
                        return Err(Error::Scanner(ScannerError::InvalidToken(
                            self.line,
                            TokenType::Or, //TODO formartar erro corretamente
                            self.start_token,
                            self.end_token,
                        )));
                    }
                }
                '/' => self.slash_or_comment(),
                ' ' | '\r' | '\t' => Blank,
                '\n' => self.newline(),
                '"' => {
                    if let Some(string) = self.string() {
                        string
                    } else {
                        return Err(Error::Scanner(ScannerError::UnterminatedString(self.line)));
                    }
                }
                c => {
                    if is_digit(c) {
                        if let Some(number) = self.number() {
                            number
                        } else {
                            return Err(Error::Scanner(ScannerError::InvalidNumber(
                                self.line,
                                TokenType::String(self.consumed().to_string()),
                                self.start_token,
                                self.end_token,
                            )));
                        }
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
                    _ => self.add_token(token),
                }
            } else {
                break;
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

    fn match_char_or_else_or_else(
        &mut self,
        expected: char,
        tt1: TokenType,
        tt2: TokenType,
        tt3: TokenType,
    ) -> TokenType {
        if Some(expected) == self.peek() {
            self.advance();
            if Some(expected) == self.peek() {
                self.advance();
                tt1
            } else {
                self.advance();
                tt2
            }
        } else {
            tt3
        }
    }

    fn slash_or_comment(&mut self) -> TokenType {
        if self.match_char('/') {
            self.advance();
            // self.take_while(|ch| ch != '\n');
            while self.peek() != Some('\n') {
                self.advance();
            }
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
        self.get_keyword(&text)
            .unwrap_or_else(|| TokenType::Identifier(text.into()))
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
            "console.log" => Some(Print),
            "return" => Some(Return),
            "this" => Some(This),
            "true" => Some(True),
            "const" => Some(Const),
            "number" => Some(Num),
            "string" => Some(Str),
            "boolean" => Some(Bool),
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
