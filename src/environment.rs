use crate::expr::Value;
use std::collections::HashMap;
#[derive(Debug, Default, Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Box<Option<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Environment>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Box::new(enclosing),
        }
    }
    pub fn define(&mut self, identifier: String, value: Value) {
        self.values.insert(identifier, value);
    }

    pub fn get(&self, identifier: &str) -> Option<&Value> {
        if self.values.contains_key(identifier) {
            self.values.get(identifier)
        } else {
            if self.enclosing.is_some() {
                self.enclosing.unwrap().get(identifier)
            } else {
                None
            }
        }
    }
}
