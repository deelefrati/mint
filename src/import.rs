use std::collections::HashMap;

use crate::{expr::Value, semantic_analyzer::Type, token_type::VarType};

#[derive(PartialEq, Clone, Debug)]
pub struct Import {
    pub name: String,
    pub t: Type,
    pub v: Value,
}

impl Import {
    fn new(name: &str, t: Type, v: Value) -> Self {
        Self {
            name: name.to_string(),
            t,
            v,
        }
    }

    pub fn imports() -> HashMap<String, Vec<Self>> {
        let mut imports = HashMap::default();

        imports.insert(
            "\"MintEqual\"".to_string(),
            vec![Import::new(
                "equal",
                Type::MintFun("equal".to_string(), VarType::Boolean, 2),
                Value::MintFun("equal".to_string(), Type::Bool),
            )],
        );

        imports
    }
}

pub type Module = HashMap<String, Vec<Import>>;
