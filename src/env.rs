use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Default)]
pub struct Env<T> {
  parent: Option<Rc<RefCell<Env<T>>>>,
  pub vars: HashMap<String, T>,
}

impl<T: Clone> Env<T> {
  pub fn new() -> Self {
    {
      Env {
        vars: HashMap::new(),
        parent: None,
      }
    }
  }

  pub fn update(&mut self, data: Rc<RefCell<Self>>) {
    self.vars.extend(
      data
        .borrow()
        .vars
        .iter()
        .map(|(k, v)| (k.clone(), v.clone())),
    );
  }

  pub fn extend(parent: Rc<RefCell<Self>>) -> Env<T> {
    Env {
      vars: HashMap::new(),
      parent: Some(parent),
    }
  }

  pub fn get(&self, name: &str) -> Option<T> {
    match self.vars.get(name) {
      Some(value) => Some(value.clone()),
      None => self
        .parent
        .as_ref()
        .and_then(|o| o.borrow().get(name)),
    }
  }

  pub fn set(&mut self, name: &str, val: T) {
    self.vars.insert(name.to_string(), val);
  }
}
