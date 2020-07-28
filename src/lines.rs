pub fn create_lines_vec(source_code: &str) -> Vec<(usize, usize)> {
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
