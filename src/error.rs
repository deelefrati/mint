#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Input(String),
    Scanner(ScannerError),
    Parse,
    Runtime,
    Unexpected,
}
#[derive(Debug, PartialEq, Clone)]
pub enum ScannerError {
    InvalidToken(usize, String, usize, usize),
    UnterminatedString(usize),
}

#[allow(dead_code)]
enum Color {
    Reset,
    White,
    Red,
    Green,
    Blue,
    Yellow,
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Color::*;
        match self {
            Reset => write!(f, "\x1b[0m"),
            White => write!(f, "\x1b[1;37m"),
            Red => write!(f, "\x1b[1;31m"),
            Green => write!(f, "\x1b[1;32m"),
            Blue => write!(f, "\x1b[1;34m"),
            Yellow => write!(f, "\x1b[1;33m"),
        }
    }
}

impl Error {
    fn blue_pipe(&self) -> String {
        format!("{}|{}", Color::Blue, Color::Reset)
    }

    pub fn show_error(
        &self,
        file: Option<&str>,
        source_vec: Option<&[String]>,
    ) {
        if let Some(filename) = file {
            eprintln!(
                "{}Error in file {}'{}':",
                Color::Red,
                Color::Reset,
                filename
            )
        } else {
            eprintln!("{}Error{}:", Color::Red, Color::Reset)
        }
        eprintln!(
            "{} {}",
            self.blue_pipe(),
            self.format_error(source_vec)
        );
    }

    fn format_error(
        &self,
        source_vec: Option<&[String]>,
    ) -> String {
        use Error::*;

        match self {
            Input(reason) => format!(
                "{}Input error: {}\n{}",
                Color::White,
                reason,
                self.blue_pipe(),
            ),
            Unexpected => format!(
                "{}Unexpected fail: Something went wrong while parsing the file{}",
                Color::White,
                Color::Reset
            ),
            Scanner(scanner_error) => {
                self.format_scanner_error(scanner_error, source_vec.unwrap())
            }
            _ => unimplemented!("Not implemented yet"),
        }
    }

    fn format_scanner_error(
        &self,
        error: &ScannerError,
        source_vec: &[String],
    ) -> String {
        use ScannerError::*;
        match error {
            InvalidToken(line,note, line_start, line_end) => format!(
                "{}Syntax error in line {} from column {} to {}: \n{}\n{} '{}'\n{}{}\n{} {}Reason: {}{}",
                Color::White,
                line,
                line_start,
                line_end,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line -1).unwrap(), 
                self.blue_pipe(),
                self.print_marker(*line_start, *line_end),
                self.blue_pipe(),
                Color::Yellow,
                note,
                Color::Reset
            ),
            UnterminatedString(line) => format!(
                "{}Unterminated String error from line {} to the end of file:\n{} \n{}'{}'\n{}\n{} {}Note: Every string must start and finish with a quotation mark, (e.g \"string\"). Maybe you forgot one?{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line -1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
        }
    }

    fn print_marker(&self, start: usize, end: usize ) -> String{
        let mut arrow: String = "  ".to_string();
        for i in 0..end {
            if i >= start {
                arrow.push('^');
            }else{
                arrow.push(' ');
            }
        }
        arrow
    }

}


