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
use std::path::Path;
use std::rc::Rc;

const MAIN_FUNC_NAME: &str = "main";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Number,
}

#[derive(Debug, Clone)]
pub struct Pointer<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub data_type: DataType,
}
pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub fpm: inkwell::passes::PassManager<FunctionValue<'ctx>>,
    pub env: Rc<RefCell<Env<Pointer<'ctx>>>>,
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
        Some(p) => {
            if p.data_type == DataType::Number {
                let typ = compiler.context.f64_type();
                compiler.builder.build_load(typ, p.ptr, sym)
            } else {
                return Err(format!("Cannot load symbol: {}", sym));
            }
        }
        None => return Err(format!("Undefined symbol: {}", sym)),
    };
    Ok(x.as_any_value_enum())
}

fn compile_function_prototype<'a>(
    compiler: &'a Compiler,
    func_proto: &'a Vec<Object>,
) -> Result<FunctionValue<'a>, String> {
    let func_name = match &func_proto[0] {
        Object::Symbol(s) => s,
        _ => return Err("Expected symbol".to_string()),
    };

    let func_params = match &func_proto[1] {
        Object::List(l) => l,
        _ => return Err("Expected list".to_string()),
    };

    let func_type = compiler.context.f64_type().fn_type(
        &vec![compiler.context.f64_type().into(); func_params.len()],
        false,
    );
    let func = compiler.module.add_function(func_name, func_type, None);
    func.get_param_iter().enumerate().for_each(|(i, p)| {
        p.set_name(&func_params[i].to_string());
    });

    Ok(func)
}

fn compile_define_function<'a>(
    compiler: &'a Compiler,
    func_proto: &'a Vec<Object>,
    func_body: &'a Object,
) -> Result<FloatValue<'a>, String> {
    let func = match compile_function_prototype(compiler, func_proto) {
        Ok(func) => func,
        Err(e) => return Err(e),
    };

    let entry = compiler.context.append_basic_block(func, "entry");
    compiler.builder.position_at_end(entry);

    Ok(compiler.context.f64_type().const_zero())
}

fn compile_define_obj<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>
) -> Result<FloatValue<'a>, String> {
    if list.len() != 2 {
        return Err(format!("Expected 1 argument, found {}", list.len() - 1));
    }

    let name = match &list[0] {
        Object::Symbol(s) => s,
        _ => return Err("Expected symbol".to_string()),
    };

    let val = match &list[1] {
        Object::Number(n) => compile_number(compiler, n),
        _ => return Err("Expected number".to_string()),
    }?;

    let ptr = compiler.builder.build_alloca(compiler.context.f64_type(), name);
    compiler.builder.build_store(ptr, val);

    compiler.env.borrow_mut().set(name, Pointer { ptr, data_type: DataType::Number });

    Ok(compiler.context.f64_type().const_zero())
}

fn compile_define<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> Result<FloatValue<'a>, String> {
    if list.len() != 3 {
        return Err(format!("Expected 2 arguments, found {}", list.len() - 1));
    }

    match &list[1] {
        Object::Symbol(s) => compile_define_obj(compiler, &list),
        Object::List(l) => compile_define_function(compiler, l, &list[2]),
        _ => return Err("Expected symbol".to_string()),
    }
}

fn compile_if<'a>(compiler: &'a Compiler, list: &'a Vec<Object>) -> Result<FloatValue<'a>, String> {
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

fn compile_binary_expr<'a>(
    binary_op: &str,
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> Result<FloatValue<'a>, String> {
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

    let val = match binary_op {
        "+" => compiler.builder.build_float_add(left, right, "addtmp"),
        "-" => compiler.builder.build_float_sub(left, right, "subtmp"),
        "*" => compiler.builder.build_float_mul(left, right, "multmp"),
        "/" => compiler.builder.build_float_div(left, right, "divtmp"),
        ">" | "<" | ">=" | "<=" | "==" | "!=" => {
            let op = match binary_op {
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
            "define" => compile_define(compiler, list),
            "if" => compile_if(compiler, list),
            "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<=" | "==" | "!=" => {
                compile_binary_expr(s, compiler, list)
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
        compiler.context.i64_type().fn_type(&[], false),
        None,
    );

    let main_block = compiler.context.append_basic_block(main_func, "entry");
    compiler.builder.position_at_end(main_block);

    match obj {
        Object::List(list) => {
            let list_len = list.len();
            let mut idx = 0;
            for obj in list {
                let val = compile_obj(&compiler, &obj)?;
                debug!("Got float value: {:?}", val);
                let int_val = compiler.builder.build_float_to_signed_int(
                    val,
                    compiler.context.i64_type(),
                    "inttmp",
                );
                idx += 1;
                if idx == list_len {
                    compiler.builder.build_return(Some(&int_val));
                }
            }
        }
        _ => debug!("{}", obj),
    }

    compiler.fpm.run_on(&main_func);
    compiler.module.print_to_stderr();
    compiler.module.print_to_file(Path::new("main.ll")).unwrap();

    Ok(())
}
