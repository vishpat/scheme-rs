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
use inkwell::values::FloatValue;
use inkwell::AddressSpace;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

const LIST_PREFIX: &str = "l_";
const FUNC_1_PREFIX: &str = "f1_";
const FUNC_2_PREFIX: &str = "f2_";

pub fn compile_function_prototype<'a>(
    compiler: &'a Compiler,
    func_proto: &'a Object,
) -> CompileResult<'a> {
    let func_proto = match func_proto {
        Object::List(l) => l,
        _ => return Err("Expected list".to_string()),
    };

    let func_name = match &func_proto[0] {
        Object::Symbol(s) => s,
        _ => return Err("Expected symbol".to_string()),
    };

    let func_params = func_proto[1..].to_vec();
    debug!(
        "Compiling function prototype with parameters: {:?}",
        func_params);

    let mut func_param_types = vec![];
    for param in func_params.iter() {
        match param {
            Object::Symbol(s) => {
                if s.starts_with(LIST_PREFIX) {
                    func_param_types.push(
                        compiler
                            .node_type
                            .ptr_type(
                                AddressSpace::default(),
                            )
                            .into(),
                    );
                }
                if s.starts_with(FUNC_1_PREFIX) {
                    func_param_types.push(
                        compiler.func1_obj_type.into(),
                    );
                }
                if s.starts_with(FUNC_2_PREFIX) {
                    func_param_types.push(
                        compiler.func2_obj_type.into(),
                    );
                } else {
                    func_param_types
                        .push(compiler.float_type.into());
                }
            }
            _ => {
                return Err(format!(
                    "Expected symbol, found: {:?}",
                    param
                ))
            }
        }
    }

    let func_type = compiler
        .float_type
        .fn_type(func_param_types.as_slice(), false);
    let func = compiler
        .module
        .add_function(func_name, func_type, None);

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
    debug!(
        "Compiling function prototype: {:?}",
        func_proto
    );
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

    for p in
        func_proto.into_function_value().get_param_iter()
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
                .build_alloca(compiler.float_type, &name);
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
            let ptr = compiler.builder.build_alloca(
                compiler
                    .node_type
                    .ptr_type(AddressSpace::default()),
                &name,
            );
            compiler.builder.build_store(ptr, p);
            sym_tables.borrow_mut().add_symbol_value(
                name.as_str(),
                Pointer {
                    ptr,
                    data_type: DataType::List,
                },
            );
        } else if p.is_struct_value() {
            let func_obj = p.into_struct_value();
            let name = func_obj
                .get_name()
                .to_str()
                .ok()
                .map(|s| s.to_string())
                .unwrap();

            debug!("Processing function object {}", name);

            let ptr = match name.as_str() {
                "func1_obj" => {
                    compiler.builder.build_alloca(
                        compiler.func1_obj_type.ptr_type(
                            AddressSpace::default(),
                        ),
                        &name,
                    )
                }
                "func2_obj" => {
                    compiler.builder.build_alloca(
                        compiler.func2_obj_type.ptr_type(
                            AddressSpace::default(),
                        ),
                        &name,
                    )
                }
                _ => {
                    return Err(format!(
                        "Expected func1_obj or func2_obj, found: {}",
                        name
                    ))
                }
            };
            compiler.builder.build_store(ptr, p);
            sym_tables.borrow_mut().add_symbol_value(
                name.as_str(),
                Pointer {
                    ptr,
                    data_type: DataType::List,
                },
            );
        } else {
            return Err(format!(
                "Expected float or pointer, found: {:?}",
                p
            ));
        }
    }

    let val = match func_body {
        Object::List(l) => {
            compile_list(compiler, l, sym_tables)?
                .into_float_value()
        }
        Object::Number(n) => {
            compile_number(compiler, n)?.into_float_value()
        }
        Object::Symbol(s) => {
            process_symbol(compiler, s, sym_tables)?
                .into_float_value()
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
    Ok(compiler.float_type.const_zero())
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

    let func = compiler
        .module
        .get_function(func_name)
        .unwrap_or_else(|| {
            panic!("Function {} not found", func_name)
        });

    debug!("Processing function call {}", func_name);

    let processed_args = list[1..]
        .iter()
        .map(|a| compile_obj(compiler, a, sym_tables))
        .collect::<Result<Vec<AnyValueEnum>, String>>()?;

    let mut compiled_args = vec![];
    for arg in processed_args.iter() {
        if arg.is_float_value() {
            compiled_args
                .push(arg.into_float_value().into());
        } else if arg.is_pointer_value() {
            compiled_args
                .push(arg.into_pointer_value().into());
        } else {
            return Err(format!(
                "Expected float or pointer, found: {:?}",
                arg
            ));
        }
    }

    let func_call = compiler.builder.build_call(
        func,
        compiled_args.as_slice(),
        "calltmp",
    );

    Ok(func_call
        .try_as_basic_value()
        .left()
        .map(|v| v.into_float_value())
        .unwrap()
        .as_any_value_enum())
}
