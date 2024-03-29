use crate::env::Env;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
  Void,
  Number(f64),
  Bool(bool),
  String(String),
  Symbol(String),
  FuncObj1Param(String),
  FuncObj2Param(String),
  ListParam(String),
  Lambda(
    Vec<String>,
    Vec<Object>,
    Rc<RefCell<Env<Object>>>,
  ),
  List(Vec<Object>),
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Object::Void => write!(f, "Void"),
      Object::Number(n) => write!(f, "{}", n),
      Object::Bool(b) => write!(f, "{}", b),
      Object::Symbol(s) => write!(f, "{}", s),
      Object::String(s) => write!(f, "{}", s),
      Object::FuncObj1Param(func) => write!(f, "{}", func),
      Object::FuncObj2Param(func) => write!(f, "{}", func),
      Object::ListParam(param) => write!(f, "{}", param),
      Object::Lambda(params, body, _) => {
        write!(f, "Lambda(")?;
        for param in params {
          write!(f, "{} ", param)?;
        }
        write!(f, ")")?;
        for expr in body {
          write!(f, " {}", expr)?;
        }
        Ok(())
      }
      Object::List(list) => {
        write!(f, "(")?;
        for (i, obj) in list.iter().enumerate() {
          if i > 0 {
            write!(f, " ")?;
          }
          write!(f, "{}", obj)?;
        }
        write!(f, ")")
      }
    }
  }
}
