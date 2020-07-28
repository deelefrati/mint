#![allow(dead_code)]
use crate::error::{Error, ScannerError};
use crate::token::Token;
use crate::token_type::TokenType;
use std::vec::Vec;

#[derive(Debug, PartialEq, Clone)]
pub struct Scanner<'a> {
	source: &'a str,
	tokens: Vec<Token>,
	start: usize,
	current: usize,
	line: usize,
}

use TokenType::*;
impl<'a> Scanner<'a> {
	pub fn new(source: &'a str) -> Self {
		Self {
			source,
			tokens: vec![],
			start: 0,
			current: 0,
			line: 1,
		}
	}

	fn is_at_end(&self) -> bool {
		self.current >= self.source.len()
	}

	fn scan_token(&mut self) -> Result<TokenType, Error> {
		use TokenType::*;
		let c = self.advance()?;
		match c {
			'(' => Ok(LeftParen),
			')' => Ok(RightParen),
			'{' => Ok(LeftBrace),
			'}' => Ok(RightBrace),
			',' => Ok(Comma),
			'.' => Ok(Dot),
			'-' => Ok(Minus),
			'+' => Ok(Plus),
			'*' => Ok(Star),
			'!' => match self.match_char('=') {
				Ok(true) => match self.match_char('=') {
					Ok(true) => Ok(BangEqualEqual),

					Ok(false) => Ok(BangEqual),
					Err(error) => Err(error),
				},
				Ok(false) => Ok(Bang),
				Err(error) => Err(error),
			},
			'=' => match self.match_char('=') {
				Ok(true) => match self.match_char('=') {
					Ok(true) => Ok(EqualEqualEqual),

					Ok(false) => Ok(EqualEqual),
					Err(error) => Err(error),
				},
				Ok(false) => Ok(Equal),
				Err(error) => Err(error),
			},
			'<' => match self.match_char('=') {
				Ok(true) => Ok(LessEqual),
				Ok(false) => Ok(Less),
				Err(error) => Err(error),
			},
			'>' => match self.match_char('=') {
				Ok(true) => Ok(GreaterEqual),
				Ok(false) => Ok(Greater),
				Err(error) => Err(error),
			},
			'&' => match self.match_char('&') {
				Ok(true) => Ok(And),
				Ok(false) => {
					Err(Error::Scanner(ScannerError::InvalidToken(
						self.line,
						std::string::String::from("Did you mean  \"&&\""), //TODO formartar erro corretamente
						0,
						0,
					)))
				}
				Err(error) => Err(error),
			},
			'|' => match self.match_char('|') {
				Ok(true) => Ok(Or),
				Ok(false) => {
					Err(Error::Scanner(ScannerError::InvalidToken(
						self.line,
						std::string::String::from("Did you mean  \"||\""), //TODO formartar erro corretamente
						0,
						0,
					)))
				}
				Err(error) => Err(error),
			},
			'/' => self.slash_or_comment(),
			' ' | '\r' | '\t' => Ok(Blank),
			'\n' => Ok(self.newline()),
			'"' => self.string(),
			_ => {
				if self.is_digit(c) {
					self.number()
				} else if self.is_alpha(c) {
					self.identifier()
				} else {
					// FIXME counters
					Err(Error::Scanner(ScannerError::InvalidToken(
						self.line,
						"".to_string(),
						0,
						0,
					)))
				}
			}
		}
	}

	pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Error> {
		while !self.is_at_end() {
			self.start = self.current;
			match self.scan_token() {
				Ok(token) => match token {
					NewLine | Blank | Comment => (),
					_ => self.add_token(token),
				},
				Err(scanner_error) => return Err(scanner_error),
			}
		}
		self.add_token(Eof);
		Ok(&self.tokens)
	}

	fn add_token(&mut self, tt: TokenType) {
		self.tokens.push(Token::new(tt, self.line, 0, 0));
	}

	fn advance(&mut self) -> Result<char, Error> {
		if let Some(c) = self.source.chars().nth(self.current) {
			self.current += 1;
			Ok(c)
		} else {
			Err(Error::Unexpected)
		}
	}

	fn match_char(&mut self, expected: char) -> Result<bool, Error> {
		if let Some(c) = self.source.chars().nth(self.current) {
			if self.is_at_end() {
				Ok(false)
			} else if c != expected {
				Ok(false)
			} else {
				self.advance()?;
				Ok(true)
			}
		} else {
			Err(Error::Unexpected)
		}
	}

	fn slash_or_comment(&mut self) -> Result<TokenType, Error> {
		match self.match_char('/') {
			Ok(true) => {
				self.advance()?;
				while self.peek_char() != '\n' && !self.is_at_end() {
					self.advance()?;
				}
				Ok(TokenType::Comment)
			}
			Ok(false) => Ok(TokenType::Slash),
			Err(error) => Err(error),
		}
	}

	fn peek_char(&self) -> char {
		if self.is_at_end() {
			'\0'
		} else {
			match self.source.chars().nth(self.current) {
				Some(c) => c,
				None => panic!("Lexer::match_char failed"),
			}
		}
	}

	fn is_digit(&self, c: char) -> bool {
		c >= '0' && c <= '9'
	}

	fn is_alphanumeric(&self, c: char) -> bool {
		self.is_alpha(c) || self.is_digit(c)
	}

	fn newline(&mut self) -> TokenType {
		self.line += 1;
		TokenType::NewLine
	}

	fn string(&mut self) -> Result<TokenType, Error> {
		let starting_line = self.line;
		while self.peek_char() != '"' && !self.is_at_end() {
			if self.peek_char() == '\n' {
				self.line += 1;
			}
			self.advance()?;
		}

		if self.is_at_end() {
			return Err(Error::Scanner(ScannerError::UnterminatedString(
				starting_line,
			)));
		} else {
			self.advance()?;
		}

		let string_value = self.source_substring(self.start + 1, self.current - 1);
		Ok(TokenType::String(std::string::String::from(string_value)))
	}

	fn source_substring(&self, start: usize, finish: usize) -> &str {
		&self.source[start..finish]
	}

	fn is_alpha(&self, c: char) -> bool {
		(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
	}

	fn number(&mut self) -> Result<TokenType, Error> {
		while self.is_digit(self.peek_char()) {
			self.advance()?;
		}

		if self.peek_char() == '.' && self.is_digit(self.peek_char()) {
			self.advance()?;
			while self.is_digit(self.peek_char()) {
				self.advance()?;
			}

			if self.peek_char() == '.' {
				self.advance()?;
				while self.is_digit(self.peek_char()) {
					self.advance()?;
				}
				return Err(Error::Scanner(ScannerError::InvalidToken(
					self.line,
					format!(
						"Failed parsing number {}",
						self.source_substring(self.start, self.current)
					),
					0,
					0,
				)));
			}
		} else if self.peek_char() == ',' {
			return Err(Error::Scanner(ScannerError::InvalidToken(
				self.line,
				"Numbers are formatted with '.' instead of ','".to_string(),
				0,
				0,
			)));
		}

		let str_number = self.source_substring(self.start, self.current);
		if let Ok(number) = str_number.parse() {
			Ok(TokenType::Number(number))
		} else {
			Err(Error::Scanner(ScannerError::InvalidToken(
				self.line,
				format!("Failed parsing number {}", str_number),
				0,
				0,
			)))
		}
	}

	fn identifier(&mut self) -> Result<TokenType, Error> {
		while self.is_alphanumeric(self.peek_char()) {
			self.advance()?;
		}

		let text = self.source_substring(self.start, self.current);
		if let Some(tt) = self.is_keyword(&text) {
			Ok(tt)
		} else {
			Ok(TokenType::Identifier(std::string::String::from(text)))
		}
	}

	fn is_keyword(&self, token: &str) -> Option<TokenType> {
		match token {
			"class" => Some(Class),
			"else" => Some(Else),
			"false" => Some(False),
			"fun" => Some(Function),
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
