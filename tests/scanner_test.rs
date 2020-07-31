use mint::scanner::Scanner;
#[test]
fn number_finishing_with_dot() {
    let string = "123.";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_err(), true)
}

#[test]
fn number_with_two_dots() {
    let string = "123.2.2";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_err(), true)
}

#[test]
fn float_ok() {
    let string = "123.3";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_ok(), true)
}

#[test]
fn integer_ok() {
    let string = "123";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_ok(), true)
}
