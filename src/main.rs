use std::env::args;
use std::process::exit;
use std::string::String;
mod error;
mod token;
mod token_type;
use error::Error;

fn main() {
	let mut arguments = args();
	if arguments.len() > 2 {
		let error = Error::Input(String::from("Too many arguments."));
		error.show_error("");
		exit(1);
	} else {
		if let Some(path) = arguments.nth(1) {
			run_file(&path)
		} else {
			let error = Error::Input(String::from("Too few  arguments were passed."));
			error.show_error("");
			exit(1);
		}
	}

	fn run_file(source_code: &str) {
		println!("{}", source_code);
	}
}
