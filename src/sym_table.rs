use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use inkwell::values::PointerValue;
use log::debug;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
  Number,
  List,
  FuncObj1,
  FuncObj2,
}

#[derive(Debug, Clone)]
pub struct Pointer<'ctx> {
  pub ptr: PointerValue<'ctx>,
  pub data_type: DataType,
}

pub struct SymTable<'ctx> {
  parent: Option<Rc<RefCell<SymTable<'ctx>>>>,
  symbols: HashMap<String, Pointer<'ctx>>,
}

impl<'ctx> SymTable<'ctx> {
  pub fn new(parent: Option<Rc<RefCell<SymTable<'ctx>>>>) -> Self {
    Self {
      parent: parent,
      symbols: HashMap::new(),
    }
  }

  pub fn add_symbol_value(
    &mut self,
    name: &str,
    ptr: Pointer<'ctx>,
  ) {
    debug!("Adding symbol {} val: {:?}", name, ptr);
    self.symbols.insert(name.to_string(), ptr);
  }

  pub fn get_symbol_value(
    &self,
    name: &str,
  ) -> Option<Pointer<'ctx>> {
    while let Some(table) = self.parent.as_ref() {
      if let Some(ptr) = table.borrow().symbols.get(name) {
        debug!("Found symbol {} val: {:?}", name, ptr);
        return Some(ptr.clone());
      }
    } 

    None
  }
}
