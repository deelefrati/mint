use crate::{error::runtime::RuntimeError, expr::Value, interpreter::Interpreter, token::Token};
use std::collections::HashMap;
#[derive(Clone, PartialEq, Debug)]
pub struct MintType {
    pub name: Token,
}

impl MintType {
    pub fn call(&self, args: &[(String, Value)]) -> MintInstance {
        MintInstance::new(self.clone(), args)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct MintInstance {
    pub mint_type: MintType,
    pub fields: HashMap<String, Value>,
}

impl MintInstance {
    pub fn new(mint_type: MintType, args: &[(String, Value)]) -> Self {
        let instance = Self {
            mint_type,
            fields: HashMap::default(),
        };
        args.iter().for_each(|(id, value)| {
            instance.fields.insert(id.to_string(), *value);
        });
        instance
    }

    //pub fn get(token: Token) -
}
