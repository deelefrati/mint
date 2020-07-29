use mint::create_lines_vec;
use mint::error::Error;
use std::env::args;
use std::fs::read_to_string;
use std::process::exit;
use std::string::String;

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
            println!("{:?}", lines_vec);
            let mut scan = mint::scanner::Scanner::new(&source_code);
            let tokens = scan.scan_tokens();
            match tokens {
                Ok(vec) => println!("{:?}", vec),
                Err(error) => error.show_error(Some(path), Some(&lines_vec), Some(&source_code)),
            }
        }
    }
}
