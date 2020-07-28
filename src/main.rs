use error::Error;
use std::env::args;
use std::fs::read_to_string;
use std::process::exit;
use std::string::String;
use std::vec::Vec;
mod error;
mod scanner;
mod token;
mod token_type;

fn main() {
	let mut arguments = args();
	if arguments.len() > 2 {
		let error = Error::Input(String::from("Too many arguments."));
		error.show_error(None, None, None);
		exit(1);
	} else {
		if let Some(path) = arguments.nth(1) {
			run_file(&path)
		} else {
			let error = Error::Input(String::from("Too few  arguments were passed."));
			error.show_error(None, None, None);
			exit(1);
		}
	}

	fn run_file(path: &str) {
		if let Ok(source_code) = read_to_string(path) {
			let lines_vec = create_lines_vec(&source_code);
			let mut scan = scanner::Scanner::new(&source_code);
			let tokens = scan.scan_tokens();
			match tokens {
				Ok(vec) => println!("{:?}", vec),
				Err(error) => error.show_error(Some(path), Some(&lines_vec), Some(&source_code)),
			}
		}
	}
}

fn create_lines_vec(source_code: &str) -> Vec<(usize, usize)> {
	let mut current = 0;
	let mut start_of_line = 0;
	let mut lines_vec: Vec<(usize, usize)> = vec![];
	if source_code.len() > 0 {
		while current < source_code.len() {
			if let Some(c) = source_code.chars().nth(current) {
				if c == '\n' {
					lines_vec.push((start_of_line, current));
					start_of_line = current;
				}
			}
			current += 1;
		}

		if let Some(c) = source_code.chars().nth(source_code.len() - 1) {
			if c != '\n' {
				lines_vec.push((start_of_line, current - 1));
			}
		}
	}
	lines_vec
}

#[test]
fn create_lines_vec_hello_world() {
	let string_test = "hello, world!";
	assert_eq!(create_lines_vec(string_test), [(0, 12)]);
}

#[test]
fn create_lines_vec_three_lines() {
	let string_test = "print(\"Hello, world!\")\n2 + 2\n2";
	assert_eq!(create_lines_vec(string_test), [(0, 22), (22, 28), (28, 29)]);
}

#[test]
fn create_lines_vec_three_lines_with_endline() {
	let three_lines = "print(\"Hello, world!\")\n2 + 2\n2\n";

	assert_eq!(create_lines_vec(three_lines), [(0, 22), (22, 28), (28, 30)]);
}

#[test]
fn create_lines_vec_empty() {
	let empty_vec: std::vec::Vec<(usize, usize)> = vec![];
	assert_eq!(create_lines_vec(""), empty_vec);
}
