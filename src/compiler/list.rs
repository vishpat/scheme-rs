use crate::compiler::compile_obj;
use crate::compiler::define::compile_define;
use crate::compiler::function::compile_function_call;
use crate::compiler::number::compile_number;
use crate::compiler::symbol::process_symbol;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::object::*;
use crate::sym_table::*;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

fn node_alloc<'a>(
  compiler: &'a Compiler,
  val: FloatValue<'a>,
) -> Result<PointerValue<'a>, String> {
  let node_ptr = compiler
    .builder
    .build_alloca(compiler.types.node_type, "node");

  debug!("Allocated node: {:?}", node_ptr);

  let data_ptr = compiler
    .builder
    .build_struct_gep(
      compiler.types.node_type,
      node_ptr,
      0,
      "data",
    )
    .map_err(|_e| {
      "Unable to build data pointer for struct".to_string()
    })?;
  compiler.builder.build_store(data_ptr, val);

  Ok(node_ptr)
}

fn node_data<'a>(
  compiler: &'a Compiler,
  node_ptr: PointerValue<'a>,
) -> Result<FloatValue<'a>, String> {
  let data_ptr = compiler
    .builder
    .build_struct_gep(
      compiler.types.node_type,
      node_ptr,
      0,
      "load_node_data_ptr",
    )
    .map_err(|_e| {
      "Unable to load node for car".to_string()
    })?;
  Ok(
    compiler
      .builder
      .build_load(
        compiler.types.float_type,
        data_ptr,
        "load_node_data",
      )
      .into_float_value(),
  )
}

fn node_next_ptr<'a>(
  compiler: &'a Compiler,
  node_ptr: PointerValue<'a>,
) -> Result<PointerValue<'a>, String> {
  let next_ptr = compiler
    .builder
    .build_struct_gep(
      compiler.types.node_type,
      node_ptr,
      1,
      "load_node_nxt_ptr",
    )
    .map_err(|_e| {
      "Unable to load node for cdr".to_string()
    })?;
  Ok(next_ptr)
}

fn node_next<'a>(
  compiler: &'a Compiler,
  node_ptr: PointerValue<'a>,
) -> CompileResult<'a> {
  let next_ptr = node_next_ptr(compiler, node_ptr)?;
  let val = compiler.builder.build_load(
    compiler
      .types
      .node_type
      .ptr_type(AddressSpace::default()),
    next_ptr,
    "loadtmp_cdr",
  );
  Ok(val.as_any_value_enum())
}

pub fn compile_quote<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 2 {
    return Err(format!(
      "Expected 1 argument, found {}",
      list.len() - 1
    ));
  }
  debug!("Processing quote: {:?}", list);

  match &list[1] {
    Object::List(l) => {
      if l.is_empty() {
        return Ok(
          compiler.types.node_null.as_any_value_enum(),
        );
      }

      let mut prev = compiler.types.node_null;
      for obj in l.iter().rev() {
        let ir_obj =
          compile_obj(compiler, obj, sym_tables)?;
        let node_ptr =
          node_alloc(compiler, ir_obj.into_float_value())?;

        debug!("quote: node_ptr: {:?}", node_ptr);

        let next_ptr = node_next_ptr(compiler, node_ptr)?;

        compiler.builder.build_store(next_ptr, prev);
        prev = node_ptr;
      }
      Ok(prev.as_any_value_enum())
    }
    _ => Err("Expected number".to_string()),
  }
}

pub fn compile_cons<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 3 {
    return Err(format!(
      "cons: Expected 2 arguments, found {:?}",
      list
    ));
  }

  debug!("Compiling cons: {:?}", list);
  let lhs = compile_obj(compiler, &list[1], sym_tables)?;
  let lhs_val = match lhs {
        AnyValueEnum::FloatValue(v) => {
            node_alloc(compiler, v)?
        }
        _ => {
            return Err(format!(
                "Cannot compile cons expected float value, lhs found: {:?}",
                lhs
            ))
        }
  };

  let rhs = compile_obj(compiler, &list[2], sym_tables)?;
  let rhs_val = match rhs {
        AnyValueEnum::PointerValue(v) => v,
        AnyValueEnum::FloatValue(v) => {
            node_alloc(compiler, v)?
        }
        _ => {
            return Err(format!(
                "Cannot compile cons expected pointer, rhs found: {:?}",
                rhs
            ))
        }
  };

  let cmp =
    compiler.builder.build_is_null(lhs_val, "isnulltmp");
  if cmp.eq(&compiler.types.bool_type.const_int(1, false)) {
    debug!("cons: lhs is null");
    return Ok(rhs_val.as_any_value_enum());
  } else {
    debug!("cons: lhs is not null {:?} {:?}", cmp, lhs_val);
  }

  let cmp =
    compiler.builder.build_is_null(rhs_val, "isnulltmp");
  if cmp.eq(&compiler.types.bool_type.const_int(1, false)) {
    debug!("cons: rhs is null");
    return Ok(lhs_val.as_any_value_enum());
  } else {
    debug!("cons: rhs is not null {:?} {:?}", cmp, rhs_val);
  }

  let val = node_next_ptr(compiler, lhs_val)?;
  compiler.builder.build_store(val, rhs_val);

  return Ok(lhs_val.as_any_value_enum());
}

pub fn compile_car<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 2 {
    return Err(format!(
      "car: Expected 1 argument, found {:?}",
      list
    ));
  }

  let val = compile_obj(compiler, &list[1], sym_tables)?;

  let val = match val {
    AnyValueEnum::PointerValue(v) => v,
    _ => {
      return Err(format!(
        "Cannot compile car expected pointer, found: {:?}",
        val
      ))
    }
  };

  let val = node_data(compiler, val)?;
  debug!("car returning float value: {:?}", val);
  Ok(val.as_any_value_enum())
}

pub fn compile_cdr<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 2 {
    return Err(format!(
      "cdr: Expected 1 argument, found {:?}",
      list
    ));
  }

  let val = compile_obj(compiler, &list[1], sym_tables)?;
  debug!("Compiling cdr: rhs : 1 {:?}", val);

  let val = match val {
    AnyValueEnum::PointerValue(v) => v,
    _ => {
      return Err(format!(
        "Cannot compile cdr expected pointer, found: {:?}",
        list[1]
      ))
    }
  };

  debug!("Compiling cdr: rhs : 2 {:?}", val);
  node_next(compiler, val)
}

fn compile_if<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 4 {
    return Err(format!(
      "Expected 3 arguments, found {}",
      list.len() - 1
    ));
  }
  let cond_ir =
    compile_obj(compiler, &list[1], sym_tables)?;
  let cond_bool = cond_ir.into_int_value();

  let curr_func = compiler
    .builder
    .get_insert_block()
    .unwrap()
    .get_parent()
    .expect("no parent");

  let mut then_bb =
    compiler.context.append_basic_block(curr_func, "then");
  let mut else_bb =
    compiler.context.append_basic_block(curr_func, "else");
  let merge_bb = compiler
    .context
    .append_basic_block(curr_func, "if_continue");

  compiler
    .builder
    .build_conditional_branch(cond_bool, then_bb, else_bb);

  compiler.builder.position_at_end(then_bb);
  let then_val =
    compile_obj(compiler, &list[2], sym_tables)?;

  compiler.builder.build_unconditional_branch(merge_bb);
  then_bb = compiler.builder.get_insert_block().unwrap();

  compiler.builder.position_at_end(else_bb);
  let else_val =
    compile_obj(compiler, &list[3], sym_tables)?;

  compiler.builder.build_unconditional_branch(merge_bb);
  else_bb = compiler.builder.get_insert_block().unwrap();

  compiler.builder.position_at_end(merge_bb);
  let phi = compiler
    .builder
    .build_phi(compiler.types.float_type, "iftmp");
  if then_val.is_float_value() && else_val.is_float_value()
  {
    phi.add_incoming(&[
      (&then_val.into_float_value(), then_bb),
      (&else_val.into_float_value(), else_bb),
    ]);
  } else if then_val.is_pointer_value()
    && else_val.is_pointer_value()
  {
    phi.add_incoming(&[
      (&then_val.into_pointer_value(), then_bb),
      (&else_val.into_pointer_value(), else_bb),
    ]);
  } else if then_val.is_int_value()
    && else_val.is_int_value()
  {
    phi.add_incoming(&[
      (&then_val.into_int_value(), then_bb),
      (&else_val.into_int_value(), else_bb),
    ]);
  } else {
    return Err(format!(
      "Expected then and else to be float or int, found {:?} and {:?}",
      then_val, else_val
    ));
  }
  Ok(phi.as_basic_value().as_any_value_enum())
}

fn compile_binary_expr<'a>(
  binary_op: &str,
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

  let left = match &list[1] {
    Object::Number(n) => compile_number(compiler, n)?,
    Object::Symbol(s) => {
      let val = process_symbol(compiler, s, sym_tables)?;
      match val {
        AnyValueEnum::FloatValue(v) => {
          v.as_any_value_enum()
        }
        _ => {
          return Err(format!(
            "Cannot compile lhs: {:?}",
            list[1]
          ))
        }
      }
    }
    Object::List(l) => {
      compile_list(compiler, l, sym_tables)?
    }
    _ => {
      return Err(format!(
        "Cannot compile lhs: {:?}",
        list[1]
      ))
    }
  };

  let right = match &list[2] {
    Object::Number(n) => compile_number(compiler, n)?,
    Object::Symbol(s) => {
      let val = process_symbol(compiler, s, sym_tables)?;
      match val {
        AnyValueEnum::FloatValue(v) => {
          v.as_any_value_enum()
        }
        _ => {
          return Err(format!(
            "Cannot compile lhs: {:?}",
            list[1]
          ))
        }
      }
    }
    Object::List(l) => {
      compile_list(compiler, l, sym_tables)?
    }
    _ => {
      return Err(format!(
        "Cannot compile rhs: {:?}",
        list[2]
      ))
    }
  };

  let left = left.into_float_value();
  let right = right.into_float_value();

  let val = match binary_op {
    "+" => compiler
      .builder
      .build_float_add(left, right, "addtmp")
      .as_any_value_enum(),
    "-" => compiler
      .builder
      .build_float_sub(left, right, "subtmp")
      .as_any_value_enum(),
    "*" => compiler
      .builder
      .build_float_mul(left, right, "multmp")
      .as_any_value_enum(),
    "/" => compiler
      .builder
      .build_float_div(left, right, "divtmp")
      .as_any_value_enum(),
    "mod" => compiler
      .builder
      .build_float_rem(left, right, "modtmp")
      .as_any_value_enum(),
    ">" | "<" | ">=" | "<=" | "=" | "!=" => {
      let op = match binary_op {
        ">" => FloatPredicate::UGT,
        "<" => FloatPredicate::ULT,
        ">=" => FloatPredicate::UGE,
        "<=" => FloatPredicate::ULE,
        "=" => FloatPredicate::UEQ,
        "!=" => FloatPredicate::UNE,
        _ => {
          return Err(format!(
            "Cannot compile list 1.: {:?}",
            list
          ))
        }
      };

      compiler
        .builder
        .build_float_compare(op, left, right, "cmptmp")
        .as_any_value_enum()
    }
    _ => {
      return Err(format!(
        "Cannot compile list 2.: {:?}",
        list
      ))
    }
  };
  Ok(val)
}

pub fn compile_null<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 2 {
    return Err(format!(
      "Expected 1 argument, found {:?}",
      list
    ));
  }

  let val = compile_obj(compiler, &list[1], sym_tables)?;
  debug!("Compiling null?: rhs {:?}", val);

  let val = match val {
    AnyValueEnum::PointerValue(v) => v,
    _ => {
      return Err(format!(
      "Cannot compile null? expected pointer, found: {:?}",
      list[1]
    ))
    }
  };

  let cmp =
    compiler.builder.build_is_null(val, "isnulltmp");
  debug!("Compiling null?: cmp {:?}", cmp);
  return Ok(cmp.as_any_value_enum());
}

fn compile_print<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.len() != 2 {
    return Err(format!(
      "Expected 1 argument, found {:?}",
      list
    ));
  }

  let val = compile_obj(compiler, &list[1], sym_tables)?;
  debug!("Compiling print: rhs {:?}", val);

  let val = match val {
    AnyValueEnum::FloatValue(v) => v,
    _ => {
      return Err(format!(
        "Cannot compile print expected float, found: {:?}",
        list[1]
      ))
    }
  };

  let format_str = unsafe {
    compiler.builder.build_global_string("%f", "format_str")
  };
  compiler
    .builder
    .build_call(
      compiler.printf_func,
      &[format_str.as_pointer_value().into(), val.into()],
      "printf",
    )
    .try_as_basic_value()
    .left()
    .unwrap();

  Ok(val.as_any_value_enum())
}

pub fn compile_list<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  if list.is_empty() {
    return Err("Cannot compile empty list".to_string());
  }

  debug!("Compiling list: {:?}", list);

  match &list[0] {
    Object::Symbol(s) => match s.as_str() {
      "define" => {
        compile_define(compiler, list, sym_tables)
      }
      "quote" => compile_quote(compiler, list, sym_tables),
      "null?" => compile_null(compiler, list, sym_tables),
      "cons" => compile_cons(compiler, list, sym_tables),
      "car" => compile_car(compiler, list, sym_tables),
      "cdr" => compile_cdr(compiler, list, sym_tables),
      "if" => compile_if(compiler, list, sym_tables),
      "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<="
      | "=" | "!=" | "mod" => {
        compile_binary_expr(s, compiler, list, sym_tables)
      }
      "apply" => compile_function_call(
        compiler,
        &list[1..],
        sym_tables,
      ),
      "print" => compile_print(compiler, list, sym_tables),
      _ => {
        compile_function_call(compiler, list, sym_tables)
      }
    },
    _ => Err(format!("Cannot compile list 4.: {:?}", list)),
  }
}
