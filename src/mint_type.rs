use crate::{expr::Value, token::Token, token_type::VarType};
use std::collections::HashMap;
#[derive(Clone, PartialEq, Debug, Default)]
pub struct MintType {
    pub name: Token,
    pub attrs: HashMap<String, VarType>,
}

impl MintType {
    pub fn call(&self, args: &[(Token, Value)]) -> MintInstance {
        MintInstance::new(self.clone(), args)
    }

    pub fn new(name: Token, attrs: HashMap<String, VarType>) -> Self {
        Self { name, attrs }
    }
}

#[derive(Clone, Debug)]
pub struct MintInstance {
    pub mint_type: MintType,
    pub fields: HashMap<String, Value>,
}

impl MintInstance {
    pub fn new(mint_type: MintType, args: &[(Token, Value)]) -> Self {
        let mut instance = Self {
            mint_type,
            fields: HashMap::default(),
        };
        args.iter().for_each(|(id, value)| {
            instance.fields.insert(id.lexeme(), value.clone());
        });
        instance
    }

    pub fn get(&self, name: &Token) -> Option<&Value> {
        self.fields.get(&name.lexeme())
    }
}

impl PartialEq for MintInstance {
    fn eq(&self, other: &Self) -> bool {
        if self.mint_type.name.lexeme() != other.mint_type.name.lexeme() {
            false
        } else {
            for (x_key, x_val) in self.fields.iter() {
                if x_val != other.fields.get(x_key).unwrap() {
                    return false;
                }
            }

            true
        }
    }
}
