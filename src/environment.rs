use std::{cell::RefCell, collections::HashMap, rc::Rc};
#[derive(PartialEq, Clone, Default, Debug)]
pub struct Environment<T> {
    inner: Rc<InnerEnv<T>>,
}

impl<T> Environment<T>
where
    T: Clone,
{
    pub fn new(current: HashMap<String, T>) -> Self {
        Self {
            inner: InnerEnv {
                previous: None,
                current: current.into(),
            }
            .into(),
        }
    }

    pub fn define(&mut self, identifier: String, value: T) -> Option<T> {
        self.inner.current.borrow_mut().insert(identifier, value)
    }

    pub fn get(&self, identifier: &str) -> Option<T> {
        self.inner.get(identifier)
    }

    pub fn push(&mut self) {
        self.inner = InnerEnv::new_with_prev(self.inner.clone()).into();
    }

    pub fn pop(&mut self) {
        self.inner = self.inner.previous.clone().unwrap();
    }
}

#[derive(PartialEq, Clone, Default, Debug)]
struct InnerEnv<T> {
    previous: Option<Rc<Self>>,
    current: RefCell<HashMap<String, T>>,
}

impl<T> InnerEnv<T>
where
    T: Clone,
{
    fn new_with_prev(previous: Rc<Self>) -> Self {
        Self {
            previous: Some(previous),
            current: Default::default(),
        }
    }

    fn get(&self, key: &str) -> Option<T> {
        if self.current.borrow().contains_key(key) {
            self.current.borrow().get(key).cloned()
        } else if self.previous.is_some() {
            self.previous.clone().unwrap().get(key)
        } else {
            None
        }
    }
}
