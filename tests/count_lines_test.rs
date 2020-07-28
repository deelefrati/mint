use mint;
#[test]
fn create_lines_vec_hello_world() {
    let string_test = "hello, world!";
    assert_eq!(mint::create_lines_vec(string_test), [(0, 12)]);
}

#[test]
fn create_lines_vec_three_lines() {
    let string_test = "print(\"Hello, world!\")\n2 + 2\n2";
    assert_eq!(
        mint::create_lines_vec(string_test),
        [(0, 22), (22, 28), (28, 29)]
    );
}

#[test]
fn create_lines_vec_three_lines_with_endline() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2\n";

    assert_eq!(
        mint::create_lines_vec(three_lines),
        [(0, 22), (22, 28), (28, 30)]
    );
}

#[test]
fn create_lines_vec_empty() {
    let empty_vec: std::vec::Vec<(usize, usize)> = vec![];
    assert_eq!(mint::create_lines_vec(""), empty_vec);
}
