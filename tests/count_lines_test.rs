use mint::create_lines_vec;
use mint::scanner::Scanner;
#[test]
fn create_lines_vec_three_lines() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2";

    assert_eq!(
        create_lines_vec(three_lines),
        ["print(\"Hello, world!\")", "2 + 2", "2"]
    );
}

#[test]
fn create_lines_vec_three_lines_with_empty_line() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2\n\n";

    assert_eq!(
        create_lines_vec(three_lines),
        ["print(\"Hello, world!\")", "2 + 2", "2", ""]
    );
}

#[test]
fn create_lines_vec_three_lines_with_endline() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2\n";

    assert_eq!(
        create_lines_vec(three_lines),
        ["print(\"Hello, world!\")", "2 + 2", "2"]
    );
}

#[test]
fn create_lines_vec_empty() {
    let empty_vec: std::vec::Vec<&str> = vec![];
    assert_eq!(create_lines_vec(""), empty_vec);
}

#[test]
fn empty_source_code() {
    let empty = "";
    let mut scanner = Scanner::new(empty);

    match scanner.scan_tokens() {
        Ok(vec) => assert_eq!(vec.len(), 1), // EOF
        Err(_) => assert!(false),
    }
}

#[test]
fn hello_world() {
    let string = "print(\"Hello, world!\")";
    let mut scanner = Scanner::new(string);

    match scanner.scan_tokens() {
        Ok(vec) => assert_eq!(vec.len(), 5), // print, (, "Hello, World", ), EOF
        Err(_) => assert!(false),
    }
}
