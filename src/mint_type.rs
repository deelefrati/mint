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

    pub fn new(name: Token, attrs: &[(Token, VarType)]) -> Self {
        let mut hash_attrs = HashMap::default();
        attrs.iter().for_each(|(token, var_type)| {
            hash_attrs.insert(token.lexeme(), *var_type);
        });

        Self {
            name,
            attrs: hash_attrs,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
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

    //pub fn get(token: Token) -
}
