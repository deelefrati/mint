#[allow(dead_code)]
pub enum Error {
    Input(String),
    Scanner,
    Parse,
    Runtime,
}

impl Error {
    pub fn show_error(&self, filename: &str) {
        match self {
            Error::Input(_) => eprintln!("Error "),
            _ => eprintln!("Error in file --> {}", filename),
        }
        eprintln!("  {}", self.format_error());
    }

    fn format_error(&self) -> String {
        use Error::*;
        match self {
            Input(reason) => format!("Input error: {} ", reason),
            _ => format!("Teste"),
        }
    }
}
