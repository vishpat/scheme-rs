use crate::compiler::function::compile_function_definition;
use crate::compiler::list::compile_list;
use crate::compiler::number::compile_number;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::object::*;
use crate::sym_table::*;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum::{
  FloatValue, PointerValue,
};
use inkwell::AddressSpace;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_define_obj<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 3 {
    return Err(format!(
      "Expected three arguments, found {} {:?}",
      list.len(),
      list
    ));
  }

  let name = match &list[1] {
    Object::Symbol(s) => s,
    _ => return Err("Expected symbol".to_string()),
  };

  let val = match &list[2] {
    Object::Number(n) => compile_number(compiler, n),
    Object::List(l) => {
      println!("Processing list: {:?}", l);
      compile_list(compiler, l, sym_tables)
    }
    _ => {
      return Err(format!(
        "Expected number, found: {}",
        list[1]
      ))
    }
  }?;

  match val {
    FloatValue(f) => {
      let global_val = compiler.module.add_global(
        compiler.types.float_type,
        Some(AddressSpace::default()),
        name,
      );
      global_val.set_initializer(&f);
    }
    PointerValue(p) => {
      let ptr =
        compiler.builder.build_alloca(p.get_type(), name);
      compiler.builder.build_store(ptr, p);
      debug!(
        "Adding list symbol: {} {} {:?}",
        name,
        p.get_type(),
        ptr
      );
      sym_tables.borrow_mut().add_symbol_value(
        name,
        Pointer {
          ptr,
          data_type: DataType::List,
        },
      );
    }
    _ => {
      return Err(format!(
        "Unexpected value: {:?}",
        list[1]
      ))
    }
  }

  Ok(val.as_any_value_enum())
}

pub fn compile_define<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 3 {
    return Err(format!(
      "Expected 2 arguments, found {}",
      list.len() - 1
    ));
  }
  debug!("Processing define: {:?}", list);
  match &list[1] {
    Object::Symbol(_) => {
      compile_define_obj(compiler, list, sym_tables)
    }
    Object::List(_) => {
      compile_function_definition(
        compiler, &list[1], &list[2], sym_tables,
      )?;
      Ok(
        compiler
          .types
          .float_type
          .const_zero()
          .as_any_value_enum(),
      )
    }
    _ => Err("Expected symbol".to_string()),
  }
}
