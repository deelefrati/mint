use std::vec::Vec;
pub fn create_lines_vec(source_code: &str) -> Vec<String> {
    let source_code_len = source_code.chars().count();

    let mut line: String = "".to_string();
    if source_code_len > 0 {
        let mut vec_lines: std::vec::Vec<String> = vec![];
        for c in source_code.chars() {
            if c == '\n' {
                vec_lines.push(line.clone());
                line = "".to_string();
            } else {
                line.push(c);
            }
        }
        if line != "" {
            vec_lines.push(line.clone());
        }
        vec_lines
    } else {
        vec![]
    }
}
