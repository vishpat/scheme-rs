use crate::env::*;
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

pub struct SymTables<'ctx> {
    pub tables: Vec<Env<Pointer<'ctx>>>,
}

impl<'ctx> SymTables<'ctx> {
    pub fn new() -> Self {
        Self {
            tables: vec![Env::new()],
        }
    }

    pub fn push_new_sym_table(&mut self) {
        self.tables.push(Env::new());
    }

    pub fn pop_sym_table(&mut self) {
        self.tables.pop();
    }

    pub fn add_symbol_value(
        &mut self,
        name: &str,
        ptr: Pointer<'ctx>,
    ) {
        debug!("Adding symbol {} val: {:?}", name, ptr);
        self.tables.last_mut().unwrap().set(name, ptr);
    }

    pub fn get_symbol_value(
        &self,
        name: &str,
    ) -> Option<Pointer<'ctx>> {
        let env = self.tables.last().unwrap();
        if let Some(val) = env.get(name) {
            debug!(
                "Got symbol {} val: {:?} from sym table",
                name, val
            );
            return Some(val.clone());
        }

        None
    }
}
