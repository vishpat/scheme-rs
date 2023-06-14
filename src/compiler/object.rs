use crate::compiler::list::compile_list;
use crate::compiler::number::compile_number;
use crate::compiler::symbol::process_symbol;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::compiler::Env;
use crate::object::*;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_obj<'a>(
  compiler: &'a Compiler,
  obj: &'a Object,
  sym_table: &mut Rc<RefCell<Env<'a>>>,
) -> CompileResult<'a> {
  debug!("Compiling Object: {:?}", obj);
  let val = match obj {
    Object::Number(n) => compile_number(compiler, n),
    Object::List(list) => {
      compile_list(compiler, list, sym_table)
    }
    Object::Symbol(s) => {
      let val = process_symbol(compiler, s, sym_table)?;
      match val {
        AnyValueEnum::FloatValue(v) => {
          Ok(v.as_any_value_enum())
        }
        AnyValueEnum::PointerValue(v) => {
          Ok(v.as_any_value_enum())
        }
        AnyValueEnum::FunctionValue(v) => {
          Ok(v.as_any_value_enum())
        }
        AnyValueEnum::IntValue(v) => {
          Ok(v.as_any_value_enum())
        }
        _ => Err(format!(
          "Cannot compile object for symbol: {:?}",
          obj
        )),
      }
    }
    _ => Err(format!("Cannot compile object: {:?}", obj)),
  };
  val
}
