use crate::env::*;
use crate::object::*;
use crate::parser::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::PointerValue;
use inkwell::values::{FloatValue, FunctionValue};
use inkwell::FloatPredicate;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

const MAIN_FUNC_NAME: &str = "main";

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub fpm: inkwell::passes::PassManager<FunctionValue<'ctx>>,
    pub env: Rc<RefCell<Env<PointerValue<'ctx>>>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("main");
        let fpm = PassManager::create(&module);
        let env = Rc::new(RefCell::new(Env::new()));

        Self {
            context,
            builder,
            module,
            fpm,
            env,
        }
    }
}

fn compile_number<'a>(compiler: &'a Compiler, n: &'a f64) -> Result<FloatValue<'a>, String> {
    Ok(compiler.context.f64_type().const_float(*n))
}

fn process_symbol<'ctx>(compiler: &'ctx Compiler, sym: &str) -> Result<AnyValueEnum<'ctx>, String> {
    let val = compiler.env.borrow().get(sym);
    debug!("Processing symbol {} val: {:?}", sym, val);
    let x = match val {
        Some(v) => compiler.builder.build_load(v, sym),
        None => return Err(format!("Undefined symbol: {}", sym)),
    };
    Ok(x.as_any_value_enum())
}

fn compile_list<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> Result<FloatValue<'a>, String> {
    if list.is_empty() {
        return Err("Cannot compile empty list".to_string());
    }

    debug!("Compiling list: {:?}", list);

    match &list[0] {
        Object::Symbol(s) => match s.as_str() {
            "define" => {
                if list.len() != 3 {
                    return Err(format!("Expected 2 arguments, found {}", list.len() - 1));
                }
                let name = match &list[1] {
                    Object::Symbol(s) => s,
                    _ => return Err("Expected symbol".to_string()),
                };
                let val = compile_obj(compiler, &list[2])?;
                let ptr = compiler
                    .builder
                    .build_alloca(compiler.context.f64_type(), name);
                compiler.builder.build_store(ptr, val);
                compiler.env.borrow_mut().set(name, ptr);
                debug!("Defined {} in env with {:?}", name, ptr);
                Ok(compiler.context.f64_type().const_zero())
            }
            "if" => {
                if list.len() != 4 {
                    return Err(format!("Expected 3 arguments, found {}", list.len() - 1));
                }
                let cond_ir = compile_obj(compiler, &list[1])?;
                let cond_bool = compiler.builder.build_float_compare(
                    inkwell::FloatPredicate::ONE,
                    cond_ir,
                    compiler.context.f64_type().const_zero(),
                    "ifcond",
                );

                let curr_func = compiler
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .expect("no parent");

                let mut then_bb = compiler.context.append_basic_block(curr_func, "then");
                let mut else_bb = compiler.context.append_basic_block(curr_func, "else");
                let merge_bb = compiler
                    .context
                    .append_basic_block(curr_func, "if_continue");

                compiler
                    .builder
                    .build_conditional_branch(cond_bool, then_bb, else_bb);

                compiler.builder.position_at_end(then_bb);
                let then_val = compile_obj(compiler, &list[2])?;
                compiler.builder.build_unconditional_branch(merge_bb);
                then_bb = compiler.builder.get_insert_block().unwrap();

                compiler.builder.position_at_end(else_bb);
                let else_val = compile_obj(compiler, &list[3])?;
                compiler.builder.build_unconditional_branch(merge_bb);
                else_bb = compiler.builder.get_insert_block().unwrap();

                compiler.builder.position_at_end(merge_bb);
                let phi = compiler
                    .builder
                    .build_phi(compiler.context.f64_type(), "iftmp");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }
            "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<=" | "==" | "!=" => {
                if list.len() != 3 {
                    return Err(format!("Expected 2 arguments, found {}", list.len() - 1));
                }

                let left = match &list[1] {
                    Object::Number(n) => compile_number(compiler, n)?,
                    Object::Symbol(s) => {
                        let val = process_symbol(compiler, s)?;
                        match val {
                            AnyValueEnum::FloatValue(v) => v,
                            _ => return Err(format!("Cannot compile lhs: {:?}", list[1])),
                        }
                    }
                    Object::List(l) => compile_list(compiler, l)?,
                    _ => return Err(format!("Cannot compile lhs: {:?}", list[1])),
                };

                let right = match &list[2] {
                    Object::Number(n) => compile_number(compiler, n)?,
                    Object::Symbol(s) => {
                        let val = process_symbol(compiler, s)?;
                        match val {
                            AnyValueEnum::FloatValue(v) => v,
                            _ => return Err(format!("Cannot compile lhs: {:?}", list[1])),
                        }
                    }
                    Object::List(l) => compile_list(compiler, l)?,
                    _ => return Err(format!("Cannot compile rhs: {:?}", list[2])),
                };

                let val = match s.as_str() {
                    "+" => compiler.builder.build_float_add(left, right, "addtmp"),
                    "-" => compiler.builder.build_float_sub(left, right, "subtmp"),
                    "*" => compiler.builder.build_float_mul(left, right, "multmp"),
                    "/" => compiler.builder.build_float_div(left, right, "divtmp"),
                    ">" | "<" | ">=" | "<=" | "==" | "!=" => {
                        let op = match s.as_str() {
                            ">" => FloatPredicate::UGT,
                            "<" => FloatPredicate::ULT,
                            ">=" => FloatPredicate::UGE,
                            "<=" => FloatPredicate::ULE,
                            "==" => FloatPredicate::UEQ,
                            "!=" => FloatPredicate::UNE,
                            _ => return Err(format!("Cannot compile list 1.: {:?}", list)),
                        };

                        let cmp_as_intval = compiler
                            .builder
                            .build_float_compare(op, left, right, "cmptmp");

                        compiler.builder.build_unsigned_int_to_float(
                            cmp_as_intval,
                            compiler.context.f64_type(),
                            "booltmp",
                        )
                    }
                    _ => return Err(format!("Cannot compile list 2.: {:?}", list)),
                };
                Ok(val)
            }
            _ => return Err(format!("Cannot compile list 3.: {:?}", list)),
        },
        _ => return Err(format!("Cannot compile list 4.: {:?}", list)),
    }
}

fn compile_obj<'a>(compiler: &'a Compiler, obj: &'a Object) -> Result<FloatValue<'a>, String> {
    debug!("Compiling Object: {:?}", obj);
    let val = match obj {
        Object::Number(n) => compile_number(compiler, n),
        Object::List(list) => compile_list(compiler, list),
        _ => Err(format!("Cannot compile object: {:?}", obj)),
    };
    val
}

pub fn compile_program(program: &str) -> Result<(), String> {
    let obj = parse(program).unwrap_or_else(|e| panic!("Error parsing program: {}", e));
    let context = Context::create();
    let compiler = Compiler::new(&context);

    let main_func = compiler.module.add_function(
        MAIN_FUNC_NAME,
        compiler.context.f64_type().fn_type(&[], false),
        None,
    );

    let main_block = compiler.context.append_basic_block(main_func, "entry");
    compiler.builder.position_at_end(main_block);

    let mut main_val;
    match obj {
        Object::List(list) => {
            for obj in list {
                let val = compile_obj(&compiler, &obj)?;
                main_val = val.as_any_value_enum().into_float_value();
                compiler.builder.build_return(Some(&main_val));
            }
        }
        _ => debug!("{}", obj),
    }

    compiler.fpm.run_on(&main_func);
    compiler.module.print_to_stderr();

    Ok(())
}
