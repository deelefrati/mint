use crate::semantic_analyzer::Type;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
#[derive(Clone, Default)]
pub struct SemanticEnvironment {
    inner: Rc<InnerEnv>,
}

impl SemanticEnvironment {
    pub fn new(current: HashMap<String, Type>) -> Self {
        Self {
            inner: InnerEnv {
                previous: None,
                current: current.into(),
            }
            .into(),
        }
    }

    pub fn define(&mut self, identifier: String, value: Type) -> Option<()> {
        if !self.inner.current.borrow().contains_key(&identifier) {
            self.inner.current.borrow_mut().insert(identifier, value);
            Some(())
        } else {
            None
        }
    }

    pub fn get(&self, identifier: &str) -> Option<Type> {
        self.inner.get(identifier)
    }

    pub fn push(&mut self) {
        self.inner = InnerEnv::new_with_prev(self.inner.clone()).into();
    }

    pub fn pop(&mut self) {
        self.inner = self.inner.previous.clone().unwrap();
    }
}

#[derive(Clone, Default, Debug)]
struct InnerEnv {
    previous: Option<Rc<Self>>,
    current: RefCell<HashMap<String, Type>>,
}

impl InnerEnv {
    fn new_with_prev(previous: Rc<Self>) -> Self {
        Self {
            previous: Some(previous),
            current: Default::default(),
        }
    }

    fn get(&self, key: &str) -> Option<Type> {
        if self.current.borrow().contains_key(key) {
            self.current.borrow().get(key).cloned()
        } else if self.previous.is_some() {
            self.previous.clone().unwrap().get(key)
        } else {
            None
        }
    }
}
