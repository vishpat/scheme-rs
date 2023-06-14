mod define;
mod function;
mod list;
mod number;
mod object;
mod symbol;
mod tests;

use crate::compiler::object::compile_obj;
use crate::object::*;
use crate::parser::*;
use crate::sym_table::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;

use inkwell::values::AnyValueEnum;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use log::debug;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

const MAIN_FUNC_NAME: &str = "main";

pub struct LLVMTypes<'ctx> {
  pub int_type: inkwell::types::IntType<'ctx>,
  pub float_type: inkwell::types::FloatType<'ctx>,
  pub node_type: inkwell::types::StructType<'ctx>,
  pub node_null: inkwell::values::PointerValue<'ctx>,
  pub func1_obj_type: inkwell::types::StructType<'ctx>,
  pub func1_ptr_type: inkwell::types::BasicTypeEnum<'ctx>,
  pub func2_obj_type: inkwell::types::StructType<'ctx>,
  pub func2_ptr_type: inkwell::types::BasicTypeEnum<'ctx>,
  pub bool_type: inkwell::types::IntType<'ctx>,
}

pub struct Compiler<'ctx> {
  pub context: &'ctx Context,
  pub builder: Builder<'ctx>,
  pub module: Module<'ctx>,
  pub fpm:
    inkwell::passes::PassManager<FunctionValue<'ctx>>,
  pub types: LLVMTypes<'ctx>,
  pub main_func: FunctionValue<'ctx>,
  pub printf_func: FunctionValue<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
  pub fn new(context: &'ctx Context) -> Self {
    let builder = context.create_builder();
    let module = context.create_module(MAIN_FUNC_NAME);
    let fpm = PassManager::create(&module);
    let node_type = context.opaque_struct_type("node");
    node_type.set_body(
      &[
        context.f64_type().into(),
        node_type.ptr_type(AddressSpace::default()).into(),
      ],
      false,
    );
    let node_null = node_type
      .ptr_type(AddressSpace::default())
      .const_null();
    let func1_obj_type =
      context.opaque_struct_type("func1_obj");
    let func1_ptr_type = context
      .f64_type()
      .fn_type(&[context.f64_type().into()], false)
      .ptr_type(AddressSpace::default())
      .into();
    func1_obj_type.set_body(&[func1_ptr_type], false);

    let func2_obj_type =
      context.opaque_struct_type("func2_obj");
    let func2_ptr_type = context
      .f64_type()
      .fn_type(
        &[
          context.f64_type().into(),
          context.f64_type().into(),
        ],
        false,
      )
      .ptr_type(AddressSpace::default())
      .into();
    func2_obj_type.set_body(&[func2_ptr_type], false);

    let bool_type = context.bool_type();
    let main_func = module.add_function(
      MAIN_FUNC_NAME,
      context.i64_type().fn_type(&[], false),
      None,
    );

    let i32_type = context.i32_type();
    let printf_func: FunctionValue = module.add_function(
      "printf",
      i32_type.fn_type(
        &[i32_type
          .ptr_type(AddressSpace::default())
          .into()],
        true,
      ),
      None,
    );

    let types = LLVMTypes {
      int_type: context.i64_type(),
      float_type: context.f64_type(),
      node_type,
      node_null,
      func1_obj_type,
      func1_ptr_type,
      func2_obj_type,
      func2_ptr_type,
      bool_type,
    };

    Self {
      context,
      builder,
      module,
      fpm,
      types,
      main_func,
      printf_func,
    }
  }
}

type CompileResult<'ctx> =
  Result<AnyValueEnum<'ctx>, String>;

pub fn compile_and_run_program(
  program: &str,
) -> Result<i32, String> {
  let obj = parse(program).unwrap_or_else(|e| {
    panic!("Error parsing program: {}", e)
  });
  let context = Context::create();
  let compiler = Compiler::new(&context);
  let mut sym_tables =
    Rc::new(RefCell::new(SymTable::new(None)));
  let mut obj_vec = vec![];
  let main_func = compiler.main_func;

  let main_block =
    compiler.context.append_basic_block(main_func, "entry");
  compiler.builder.position_at_end(main_block);

  match obj {
    Object::List(list) => {
      let list_len = list.len();
      let mut idx = 0;
      for obj in list {
        obj_vec.push(obj);
      }

      while idx < list_len {
        let val = compile_obj(
          &compiler,
          &obj_vec[idx],
          &mut sym_tables,
        )?;
        let ret_val = match val {
          AnyValueEnum::FloatValue(_) => {
            let val = val.into_float_value();
            compiler.builder.build_float_to_unsigned_int(
              val,
              compiler.types.int_type,
              "rettmp",
            )
          }
          _ => compiler.types.int_type.const_zero(),
        };

        idx += 1;
        if idx == list_len {
          compiler.builder.build_return(Some(&ret_val));
        }
      }
    }
    _ => debug!("{}", obj),
  }

  compiler.fpm.run_on(&main_func);
  //compiler.module.print_to_stderr();
  compiler
    .module
    .print_to_file(Path::new("main.ll"))
    .unwrap();

  println!("Running execution engine...");
  let execution_engine = compiler
    .module
    .create_jit_execution_engine(OptimizationLevel::None)
    .unwrap();

  let main = execution_engine
    .get_function_value(MAIN_FUNC_NAME)
    .unwrap();

  let ret = unsafe {
    execution_engine.run_function_as_main(main, &[])
  };

  Ok(ret)
}
