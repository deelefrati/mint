use crate::expr::Value;
use std::collections::HashMap;
#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn define(&mut self, identifier: String, value: Value) {
        self.values.insert(identifier, value);
    }

    pub fn get(&self, identifier: &str) -> Option<&Value> {
        self.values.get(identifier)
    }
}
