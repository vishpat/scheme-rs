use crate::object::*;
use crate::parser::*;
use crate::sym_table::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::{FloatValue, FunctionValue};
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::OptimizationLevel;
use log::debug;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

const MAIN_FUNC_NAME: &str = "main";

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub fpm:
        inkwell::passes::PassManager<FunctionValue<'ctx>>,
    pub sym_tables: Rc<RefCell<SymTables<'ctx>>>,
    pub int_type: inkwell::types::IntType<'ctx>,
    pub float_type: inkwell::types::FloatType<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(MAIN_FUNC_NAME);
        let fpm = PassManager::create(&module);
        let sym_tables =
            Rc::new(RefCell::new(SymTables::new()));

        Self {
            context,
            builder,
            module,
            fpm,
            sym_tables,
            int_type: context.i64_type(),
            float_type: context.f64_type(),
        }
    }
}

type CompileResult<'ctx> =
    Result<AnyValueEnum<'ctx>, String>;

fn compile_number<'a>(
    compiler: &'a Compiler,
    n: &'a f64,
) -> CompileResult<'a> {
    Ok(compiler
        .float_type
        .const_float(*n)
        .as_any_value_enum())
}

fn process_symbol<'ctx>(
    compiler: &'ctx Compiler,
    sym: &str,
) -> Result<AnyValueEnum<'ctx>, String> {
    let global_val = compiler.module.get_global(sym);
    if let Some(g) = global_val {
        let val = g.get_initializer().unwrap();
        debug!(
            "Loading global symbol: {} with value {:?}",
            sym, val
        );
        compiler.builder.build_load(
            compiler.float_type,
            g.as_pointer_value(),
            sym,
        );
        return Ok(val.as_any_value_enum());
    }

    let val =
        compiler.sym_tables.borrow().get_symbol_value(sym);

    debug!("Processing symbol {} val: {:?}", sym, val);
    let x = match val {
        Some(p) => {
            if p.data_type == DataType::Number {
                debug!("Loading symbol: {}", sym);
                compiler.builder.build_load(
                    compiler.float_type,
                    p.ptr,
                    sym,
                )
            } else {
                return Err(format!(
                    "Cannot load symbol: {}",
                    sym
                ));
            }
        }
        None => {
            return Err(format!(
                "Undefined symbol: {}",
                sym
            ))
        }
    };
    Ok(x.as_any_value_enum())
}

fn compile_function_prototype<'a>(
    compiler: &'a Compiler,
    func_proto: &'a [Object],
) -> Result<FunctionValue<'a>, String> {
    let func_name = match &func_proto[0] {
        Object::Symbol(s) => s,
        _ => return Err("Expected symbol".to_string()),
    };

    let func_params = func_proto[1..].to_vec();

    let func_type = compiler.float_type.fn_type(
        &vec![
            compiler.float_type.into();
            func_params.len()
        ],
        false,
    );
    let func = compiler
        .module
        .add_function(func_name, func_type, None);

    func.get_param_iter().enumerate().for_each(|(i, p)| {
        p.set_name(&func_params[i].to_string());
    });

    Ok(func)
}

fn compile_function_definition<'a>(
    compiler: &'a Compiler,
    func_proto: &'a [Object],
    func_body: &'a Object,
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

    let entry = compiler
        .context
        .append_basic_block(func_proto, "entry");
    compiler.builder.position_at_end(entry);
    compiler.sym_tables.borrow_mut().push_new_sym_table();

    for p in func_proto.get_param_iter() {
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
            compiler
                .sym_tables
                .borrow_mut()
                .add_symbol_value(
                    &name,
                    Pointer {
                        ptr,
                        data_type: DataType::Number,
                    },
                );
        }
    }

    let _func_body = match func_body {
        Object::List(l) => l.clone(),
        _ => {
            return Err(format!(
            "Function definition body must be a list: {:?}",
            func_body
        ))
        }
    };

    let val = compile_list(compiler, &_func_body)?
        .into_float_value();
    compiler.builder.build_return(Some(&val));
    func_proto.verify(true);

    compiler.sym_tables.borrow_mut().pop_sym_table();

    compiler.builder.position_at_end(current_bb);
    Ok(compiler.float_type.const_zero())
}

fn compile_function_call<'a>(
    compiler: &'a Compiler,
    list: &'a [Object],
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

    let compiled_args = list[1..]
        .iter()
        .map(|a| compile_obj(compiler, a))
        .collect::<Result<Vec<AnyValueEnum>, String>>()?;
    let compiled_args: Vec<BasicMetadataValueEnum> =
        compiled_args
            .into_iter()
            .map(|val| val.into_float_value().into())
            .collect();

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

fn compile_define_obj<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
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

    let mut set_global = true;
    let val = match &list[2] {
        Object::Number(n) => compile_number(compiler, n),
        Object::List(l) => {
            set_global = false;
            compile_list(compiler, l)
        }
        _ => {
            return Err(format!(
                "Expected number, found: {}",
                list[1]
            ))
        }
    }?;

    let val = val.into_float_value();
    let ptr = compiler
        .builder
        .build_alloca(compiler.float_type, name);
    compiler.builder.build_store(ptr, val);

    if set_global {
        let global_val = compiler.module.add_global(
            compiler.float_type,
            Some(AddressSpace::default()),
            name,
        );
        global_val.set_initializer(&val);
    } else {
        compiler.sym_tables.borrow_mut().add_symbol_value(
            name,
            Pointer {
                ptr,
                data_type: DataType::Number,
            },
        );
    }

    Ok(compiler.float_type.const_zero().as_any_value_enum())
}

fn compile_define<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> CompileResult<'a> {
    if list.len() != 3 {
        return Err(format!(
            "Expected 2 arguments, found {}",
            list.len() - 1
        ));
    }
    debug!("Processing define: {:?}", list);

    let mut func_proto = Vec::new();
    let is_function = match &list[1] {
        Object::List(proto) => {
            func_proto = proto.clone();
            true
        }
        _ => false,
    };

    match &list[2] {
        Object::Number(_) => {
            compile_define_obj(compiler, list)
        }
        Object::List(_) => {
            if is_function {
                compile_function_definition(
                    compiler,
                    &func_proto,
                    &list[2],
                )?;
                Ok(compiler
                    .float_type
                    .const_zero()
                    .as_any_value_enum())
            } else {
                compile_define_obj(compiler, list)
            }
        }
        _ => Err("Expected symbol".to_string()),
    }
}

fn compile_quote<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> CompileResult<'a> {
    if list.len() != 2 {
        return Err(format!(
            "Expected 1 argument, found {}",
            list.len() - 1
        ));
    }
    debug!("Processing quote: {:?}", list);

    let val = match &list[1] {
        Object::List(l) => {
            let arr_len = compiler
                .int_type
                .const_int((l.len() + 1) as u64, false);
            let arr = compiler.builder.build_array_alloca(
                compiler.float_type,
                arr_len,
                "array",
            );
            for (i, obj) in l.iter().enumerate() {
                let val = match obj {
                    Object::Number(n) => {
                        compile_number(compiler, n)
                    }
                    _ => {
                        return Err(format!(
                            "Expected number, found: {}",
                            obj
                        ))
                    }
                }?;
                let val = val.into_float_value();
                let ptr = unsafe {
                    compiler.builder.build_gep(
                        compiler.float_type,
                        arr,
                        &[compiler
                            .int_type
                            .const_int(i as u64, false)],
                        "pointer",
                    )
                };
                compiler.builder.build_store(ptr, val);
            }
            arr.as_any_value_enum()
        }
        _ => {
            return Err(format!(
                "Expected list, found: {}",
                list[1]
            ))
        }
    };

    Ok(val)
}

fn compile_if<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> CompileResult<'a> {
    if list.len() != 4 {
        return Err(format!(
            "Expected 3 arguments, found {}",
            list.len() - 1
        ));
    }
    let cond_ir = compile_obj(compiler, &list[1])?;
    let cond_ir = cond_ir.into_float_value();
    let cond_bool = compiler.builder.build_float_compare(
        inkwell::FloatPredicate::ONE,
        cond_ir,
        compiler.float_type.const_zero(),
        "ifcond",
    );

    let curr_func = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .expect("no parent");

    let mut then_bb = compiler
        .context
        .append_basic_block(curr_func, "then");
    let mut else_bb = compiler
        .context
        .append_basic_block(curr_func, "else");
    let merge_bb = compiler
        .context
        .append_basic_block(curr_func, "if_continue");

    compiler.builder.build_conditional_branch(
        cond_bool, then_bb, else_bb,
    );

    compiler.builder.position_at_end(then_bb);
    let then_val =
        compile_obj(compiler, &list[2])?.into_float_value();
    compiler.builder.build_unconditional_branch(merge_bb);
    then_bb = compiler.builder.get_insert_block().unwrap();

    compiler.builder.position_at_end(else_bb);
    let else_val =
        compile_obj(compiler, &list[3])?.into_float_value();
    compiler.builder.build_unconditional_branch(merge_bb);
    else_bb = compiler.builder.get_insert_block().unwrap();

    compiler.builder.position_at_end(merge_bb);
    let phi = compiler
        .builder
        .build_phi(compiler.float_type, "iftmp");
    phi.add_incoming(&[
        (&then_val, then_bb),
        (&else_val, else_bb),
    ]);
    Ok(phi
        .as_basic_value()
        .into_float_value()
        .as_any_value_enum())
}

fn compile_binary_expr<'a>(
    binary_op: &str,
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
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
            let val = process_symbol(compiler, s)?;
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
        Object::List(l) => compile_list(compiler, l)?,
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
            let val = process_symbol(compiler, s)?;
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
        Object::List(l) => compile_list(compiler, l)?,
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
            .build_float_add(left, right, "addtmp"),
        "-" => compiler
            .builder
            .build_float_sub(left, right, "subtmp"),
        "*" => compiler
            .builder
            .build_float_mul(left, right, "multmp"),
        "/" => compiler
            .builder
            .build_float_div(left, right, "divtmp"),
        ">" | "<" | ">=" | "<=" | "==" | "!=" => {
            let op = match binary_op {
                ">" => FloatPredicate::UGT,
                "<" => FloatPredicate::ULT,
                ">=" => FloatPredicate::UGE,
                "<=" => FloatPredicate::ULE,
                "==" => FloatPredicate::UEQ,
                "!=" => FloatPredicate::UNE,
                _ => {
                    return Err(format!(
                        "Cannot compile list 1.: {:?}",
                        list
                    ))
                }
            };

            let cmp_as_intval =
                compiler.builder.build_float_compare(
                    op, left, right, "cmptmp",
                );

            compiler.builder.build_unsigned_int_to_float(
                cmp_as_intval,
                compiler.float_type,
                "booltmp",
            )
        }
        _ => {
            return Err(format!(
                "Cannot compile list 2.: {:?}",
                list
            ))
        }
    };
    Ok(val.as_any_value_enum())
}

fn compile_list<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> CompileResult<'a> {
    if list.is_empty() {
        return Err("Cannot compile empty list".to_string());
    }

    debug!("Compiling list: {:?}", list);

    match &list[0] {
        Object::Symbol(s) => match s.as_str() {
            "define" => compile_define(compiler, list),
            "quote" => compile_quote(compiler, list),
            "if" => compile_if(compiler, list),
            "+" | "-" | "*" | "/" | ">" | "<" | ">="
            | "<=" | "==" | "!=" => {
                compile_binary_expr(s, compiler, list)
            }
            _ => compile_function_call(compiler, list),
        },
        _ => {
            return Err(format!(
                "Cannot compile list 4.: {:?}",
                list
            ))
        }
    }
}

fn compile_obj<'a>(
    compiler: &'a Compiler,
    obj: &'a Object,
) -> CompileResult<'a> {
    debug!("Compiling Object: {:?}", obj);
    let val = match obj {
        Object::Number(n) => compile_number(compiler, n),
        Object::List(list) => compile_list(compiler, list),
        Object::Symbol(s) => {
            let val = process_symbol(compiler, s)?;
            match val {
                AnyValueEnum::FloatValue(v) => {
                    Ok(v.as_any_value_enum())
                }
                _ => Err(format!(
                    "Cannot compile object: {:?}",
                    obj
                )),
            }
        }
        _ => {
            Err(format!("Cannot compile object: {:?}", obj))
        }
    };
    val
}

pub fn compile_program(
    program: &str,
) -> Result<(), String> {
    let obj = parse(program).unwrap_or_else(|e| {
        panic!("Error parsing program: {}", e)
    });
    let context = Context::create();
    let compiler = Compiler::new(&context);

    let main_func = compiler.module.add_function(
        MAIN_FUNC_NAME,
        compiler.int_type.fn_type(&[], false),
        None,
    );

    let main_block = compiler
        .context
        .append_basic_block(main_func, "entry");
    compiler.builder.position_at_end(main_block);

    match obj {
        Object::List(list) => {
            let list_len = list.len();
            let mut idx = 0;
            for obj in list {
                let val = compile_obj(&compiler, &obj)?;
                let ret_val = match val {
                    AnyValueEnum::FloatValue(_) => {
                        let val = val.into_float_value();
                        compiler
                            .builder
                            .build_float_to_unsigned_int(
                                val,
                                compiler.int_type,
                                "rettmp",
                            )
                    }
                    _ => compiler.int_type.const_zero(),
                };

                idx += 1;
                if idx == list_len {
                    compiler
                        .builder
                        .build_return(Some(&ret_val));
                }
            }
        }
        _ => debug!("{}", obj),
    }

    compiler.fpm.run_on(&main_func);
    compiler.module.print_to_stderr();
    compiler
        .module
        .print_to_file(Path::new("main.ll"))
        .unwrap();

    println!("Running execution engine...");
    let execution_engine = compiler
        .module
        .create_jit_execution_engine(
            OptimizationLevel::None,
        )
        .unwrap();

    let main = execution_engine
        .get_function_value(MAIN_FUNC_NAME)
        .unwrap();

    let ret = unsafe {
        execution_engine.run_function_as_main(main, &[])
    };

    println!("Result: {:?}", ret);
    Ok(())
}
