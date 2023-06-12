use crate::compiler::compile_obj;
use crate::compiler::list::compile_list;
use crate::compiler::number::compile_number;
use crate::compiler::symbol::process_symbol;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::object::*;
use crate::sym_table::*;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicValue;
use inkwell::values::FloatValue;
use inkwell::AddressSpace;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_function_prototype<'a>(
  compiler: &'a Compiler,
  func_proto: &'a Object,
) -> CompileResult<'a> {
  let func_proto = match func_proto {
    Object::List(l) => l,
    _ => return Err("Expected list".to_string()),
  };

  let mut list_ret = false;
  let func_name = match &func_proto[0] {
    Object::Symbol(s) => s,
    Object::ListParam(s) => {
      list_ret = true;
      s
    }
    _ => return Err("Expected symbol".to_string()),
  };

  let func_params = func_proto[1..].to_vec();
  debug!(
    "Compiling function prototype with parameters: {:?}",
    func_params
  );

  let mut func_param_types = vec![];
  for param in func_params.iter() {
    match param {
      Object::ListParam(_) => {
        func_param_types.push(
          compiler
            .types
            .node_type
            .ptr_type(AddressSpace::default())
            .into(),
        );
      }
      Object::FuncObj1Param(_) => {
        func_param_types.push(
          compiler
            .types
            .func1_obj_type
            .ptr_type(AddressSpace::default())
            .into(),
        );
      }
      Object::FuncObj2Param(_) => {
        func_param_types.push(
          compiler
            .types
            .func2_obj_type
            .ptr_type(AddressSpace::default())
            .into(),
        );
      }
      _ => {
        func_param_types
          .push(compiler.types.float_type.into());
      }
    }
  }

  debug!(
    "Function parameter types: {:?}",
    func_param_types
  );
  let func_type = if list_ret {
    compiler
      .types
      .node_type
      .ptr_type(AddressSpace::default())
      .fn_type(func_param_types.as_slice(), false)
  } else {
    compiler
      .types
      .float_type
      .fn_type(func_param_types.as_slice(), false)
  };
  let func = compiler
    .module
    .add_function(func_name, func_type, None);

  debug!("Function parameters: {:?}", func_params);
  func.get_param_iter().enumerate().for_each(|(i, p)| {
    p.set_name(&func_params[i].to_string());
  });

  Ok(func.as_any_value_enum())
}

pub fn compile_function_definition<'a>(
  compiler: &'a Compiler,
  func_proto: &'a Object,
  func_body: &'a Object,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> Result<FloatValue<'a>, String> {
  debug!("Compiling function prototype: {:?}", func_proto);

  let func_params = match func_proto {
    Object::List(l) => l,
    _ => return Err("Expected list".to_string()),
  };

  let func_params = &func_params[1..];

  let func_proto = match compile_function_prototype(
    compiler, func_proto,
  ) {
    Ok(func) => func,
    Err(e) => return Err(e),
  };

  let current_bb =
    compiler.builder.get_insert_block().unwrap();

  let entry = compiler.context.append_basic_block(
    func_proto.into_function_value(),
    "entry",
  );
  compiler.builder.position_at_end(entry);
  sym_tables.borrow_mut().push_new_sym_table();

  for (param_idx, p) in func_proto
    .into_function_value()
    .get_param_iter()
    .enumerate()
  {
    debug!("Processing function parameter {}", p);
    if p.is_float_value() {
      let param_float = p.into_float_value();
      let name = param_float
        .get_name()
        .to_str()
        .ok()
        .map(|s| s.to_string())
        .unwrap();
      let ptr = compiler
        .builder
        .build_alloca(compiler.types.float_type, &name);
      compiler.builder.build_store(ptr, p);

      sym_tables.borrow_mut().add_symbol_value(
        &name,
        Pointer {
          ptr,
          data_type: DataType::Number,
        },
      );
    } else if p.is_pointer_value() {
      let param_ptr = p.into_pointer_value();
      let name = param_ptr
        .get_name()
        .to_str()
        .ok()
        .map(|s| s.to_string())
        .unwrap();

      let ty;
      let ptr;
      let param = &func_params[param_idx];
      match param {
        Object::ListParam(_) => {
          ty = DataType::List;
          ptr = compiler.builder.build_alloca(
            compiler
              .types
              .node_type
              .ptr_type(AddressSpace::default()),
            &name,
          );
        }
        Object::FuncObj1Param(_) => {
          ty = DataType::FuncObj1;
          ptr = compiler.builder.build_alloca(
            compiler
              .types
              .func1_obj_type
              .ptr_type(AddressSpace::default()),
            &name,
          );
        }
        Object::FuncObj2Param(_) => {
          ty = DataType::FuncObj2;
          ptr = compiler.builder.build_alloca(
            compiler
              .types
              .func2_obj_type
              .ptr_type(AddressSpace::default()),
            &name,
          );
        }
        _ => {
          return Err(format!(
            "Unknown function parameter type: {}",
            name
          ));
        }
      }

      compiler.builder.build_store(ptr, p);
      sym_tables.borrow_mut().add_symbol_value(
        name.as_str(),
        Pointer { ptr, data_type: ty },
      );
    } else {
      return Err(format!(
                "Function Definition: Expected float or pointer, found: {:?}",
                p
            ));
    }
  }

  let val;
  match func_body {
    Object::List(l) => {
      let list_val = compile_list(compiler, l, sym_tables)?;
      if list_val.is_float_value() {
        val =
          list_val.into_float_value().as_basic_value_enum();
      } else if list_val.is_pointer_value() {
        val = list_val
          .into_pointer_value()
          .as_basic_value_enum();
      } else {
        return Err(format!(
          "Function definition body must be a list: {:?}",
          func_body
        ));
      }
    }
    Object::Number(n) => {
      val = compile_number(compiler, n)?
        .into_float_value()
        .as_basic_value_enum();
    }
    Object::Symbol(s) => {
      val = process_symbol(compiler, s, sym_tables)?
        .into_float_value()
        .as_basic_value_enum();
    }
    _ => {
      return Err(format!(
        "Function definition body must be a list: {:?}",
        func_body
      ))
    }
  };
  compiler.builder.build_return(Some(&val));
  func_proto.into_function_value().verify(true);

  sym_tables.borrow_mut().pop_sym_table();

  compiler.builder.position_at_end(current_bb);
  Ok(compiler.types.float_type.const_zero())
}

pub fn compile_function_call<'a>(
  compiler: &'a Compiler,
  list: &'a [Object],
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  let func_name = match &list[0] {
    Object::Symbol(s) => s,
    _ => return Err("Expected symbol".to_string()),
  };
  let processed_args = list[1..]
    .iter()
    .map(|a| compile_obj(compiler, a, sym_tables))
    .collect::<Result<Vec<AnyValueEnum>, String>>()?;

  let mut compiled_args = vec![];
  for arg in processed_args.iter() {
    if arg.is_float_value() {
      compiled_args.push(arg.into_float_value().into());
    } else if arg.is_pointer_value() {
      compiled_args.push(arg.into_pointer_value().into());
    } else if arg.is_function_value() {
      let fn_val = arg.into_function_value();
      let arg_count = fn_val.count_params();
      let ty = if arg_count == 1 {
        compiler.types.func1_obj_type
      } else {
        compiler.types.func2_obj_type
      };
      match arg_count {
        1 | 2 => {
          let func_obj_ptr = compiler
            .builder
            .build_alloca(ty, "func_obj_ptr");
          let func_ptr=
                        compiler.builder.build_struct_gep(
                            ty,
                            func_obj_ptr,
                            0,
                            "func_obj_ptr",
                        ).map_err(|_e| "Unable to build function pointer for function object".to_string())?;
          compiler.builder.build_store(
            func_ptr,
            fn_val.as_global_value().as_pointer_value(),
          );
          compiled_args.push(func_obj_ptr.into());
        }
        _ => {
          return Err(format!(
                        "Function Call: Expected function with 1 or 2 arguments, found: {}",
                        arg_count
                    ));
        }
      }
    } else {
      return Err(format!(
                "Function Call: Expected float or pointer, found: {:?}",
                arg
            ));
    }
  }

  debug!("Compiling function call: {:?}", list);

  let func = compiler.module.get_function(func_name);

  if func.is_none() {
    debug!("Function {} not found, so checking for function object", func_name);
    let func_ptr =
      sym_tables.borrow_mut().get_symbol_value(func_name);

    if func_ptr.is_none() {
      return Err(format!(
        "Function {} not found",
        func_name
      ));
    }
    let ptr = func_ptr.unwrap();
    if ptr.data_type != DataType::FuncObj1
      && ptr.data_type != DataType::FuncObj2
    {
      return Err(format!(
        "Expected function object, found: {:?}",
        ptr.data_type
      ));
    }

    let func_call = match ptr.data_type {
      DataType::FuncObj1 => {
        let val = compiler.builder.build_load(
          compiler
            .types
            .func1_obj_type
            .ptr_type(AddressSpace::default()),
          ptr.ptr,
          "loadtmp_func1_obj",
        );
        let ptr = compiler
          .builder
          .build_struct_gep(
            compiler.types.func1_obj_type,
            val.into_pointer_value(),
            0,
            "geptmp",
          )
          .map_err(|_e| {
            "Unable to load node for func1_obj".to_string()
          })?;
        let fn_ptr = compiler.builder.build_load(
          compiler.types.func1_ptr_type,
          ptr,
          "loadtmp_func1_ptr",
        );
        let fn_type = compiler.types.float_type.fn_type(
          &[compiler.types.float_type.into()],
          false,
        );
        compiler.builder.build_indirect_call(
          fn_type,
          fn_ptr.into_pointer_value(),
          compiled_args.as_slice(),
          "call_indirect_func1",
        )
      }
      DataType::FuncObj2 => {
        let val = compiler.builder.build_load(
          compiler
            .types
            .func2_obj_type
            .ptr_type(AddressSpace::default()),
          ptr.ptr,
          "loadtmp_func2_obj",
        );
        let ptr = compiler
          .builder
          .build_struct_gep(
            compiler.types.func2_obj_type,
            val.into_pointer_value(),
            0,
            "geptmp",
          )
          .map_err(|_e| {
            "Unable to load node for func2_obj".to_string()
          })?;
        let fn_ptr = compiler.builder.build_load(
          compiler.types.func2_ptr_type,
          ptr,
          "loadtmp_func2_ptr",
        );
        let fn_type = compiler.types.float_type.fn_type(
          &[
            compiler.types.float_type.into(),
            compiler.types.float_type.into(),
          ],
          false,
        );
        compiler.builder.build_indirect_call(
          fn_type,
          fn_ptr.into_pointer_value(),
          compiled_args.as_slice(),
          "call_indirect_func2",
        )
      }
      _ => return Err("Should not happen".to_string()),
    };

    debug!("Processing function call {:?}", func_call);
    Ok(
      func_call
        .try_as_basic_value()
        .left()
        .unwrap()
        .as_any_value_enum(),
    )
  } else {
    let func_call = compiler.builder.build_call(
      func.unwrap(),
      compiled_args.as_slice(),
      "call_direct",
    );

    Ok(
      func_call
        .try_as_basic_value()
        .left()
        .unwrap()
        .as_any_value_enum(),
    )
  }
}

pub fn compile_let<'a>(
  compiler: &'a Compiler,
  list: &'a Vec<Object>,
  sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
  let pairs = match list.get(1) {
    Some(Object::List(pairs)) => pairs,
    _ => {
      return Err(
        "Let: Expected list of pairs".to_string(),
      );
    }
  };
  let current_bb =
    compiler.builder.get_insert_block().unwrap();

  let entry = compiler.context.append_basic_block(
    compiler.module.get_last_function().unwrap(),
    "entry",
  );
  compiler.builder.position_at_end(entry);

  sym_tables.borrow_mut().push_new_sym_table();
  for pair in pairs {
    let pair = match pair {
      Object::List(pair) => pair,
      _ => {
        return Err(
          "Let: Expected list of pairs".to_string(),
        );
      }
    };
    let key = match pair.get(0) {
      Some(Object::Symbol(key)) => key,
      _ => {
        return Err(
          "Let: Expected symbol as key".to_string(),
        );
      }
    };
    let val = match pair.get(1) {
      Some(val) => val,
      _ => {
        return Err("Let: Expected value".to_string());
      }
    };
    let val = compile_obj(compiler, val, sym_tables)?;
    let ptr = compiler
      .builder
      .build_alloca(compiler.types.float_type, key);
    compiler
      .builder
      .build_store(ptr, val.into_float_value());

    sym_tables.borrow_mut().add_symbol_value(
      key,
      Pointer {
        ptr,
        data_type: DataType::Number,
      },
    );
  }

  let mut result = AnyValueEnum::FloatValue(
    compiler.context.f64_type().const_float(0.0),
  );

  for item in list.iter().skip(2) {
    result = compile_obj(compiler, item, sym_tables)?;
  }

  sym_tables.borrow_mut().pop_sym_table();

  match result {
    AnyValueEnum::FloatValue(f) => {
      compiler.builder.build_return(Some(&f));
    }
    _ => {
      compiler.builder.build_return(None);
    }
  }

  compiler.builder.position_at_end(current_bb);

  Ok(result)
}
