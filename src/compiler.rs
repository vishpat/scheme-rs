use crate::object::*;
use crate::parser::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::AnyValue;
use inkwell::values::{FloatValue, FunctionValue};
use inkwell::FloatPredicate;

const MAIN_FUNC_NAME: &str = "main";

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub fpm: inkwell::passes::PassManager<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("main");
        let fpm = PassManager::create(&module);

        Self {
            context,
            builder,
            module,
            fpm,
        }
    }
}

fn compile_number<'a>(compiler: &'a Compiler, n: &'a f64) -> Result<FloatValue<'a>, String> {
    Ok(compiler.context.f64_type().const_float(*n))
}

fn compile_list<'a>(
    compiler: &'a Compiler,
    list: &'a Vec<Object>,
) -> Result<FloatValue<'a>, String> {
    if list.is_empty() {
        return Err("Cannot compile empty list".to_string());
    }

    match &list[0] {
        Object::Symbol(s) => match s.as_str() {
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
                    Object::List(l) => compile_list(compiler, l)?,
                    _ => return Err(format!("Cannot compile lhs: {:?}", list[1])),
                };

                let right = match &list[2] {
                    Object::Number(n) => compile_number(compiler, n)?,
                    Object::List(l) => compile_list(compiler, l)?,
                    _ => return Err(format!("Cannot compile rhs: {:?}", list[2])),
                };

                let val = match s.as_str() {
                    "+" => compiler.builder.build_float_add(left, right, "addtmp"),
                    "-" => compiler.builder.build_float_sub(left, right, "subtmp"),
                    "*" => compiler.builder.build_float_mul(left, right, "multmp"),
                    "/" => compiler.builder.build_float_div(left, right, "divtmp"),
                    ">" | "<" | ">=" | "<=" | "==" | "!=" => {
                        let cmp_as_intval = compiler.builder.build_float_compare(
                            FloatPredicate::ULT,
                            left,
                            right,
                            "cmptmp",
                        );

                        compiler.builder.build_unsigned_int_to_float(
                            cmp_as_intval,
                            compiler.context.f64_type(),
                            "booltmp",
                        )
                    }
                    _ => return Err(format!("Cannot compile list: {:?}", list)),
                };
                Ok(val)
            }
            _ => return Err(format!("Cannot compile list: {:?}", list)),
        },
        _ => return Err(format!("Cannot compile list: {:?}", list)),
    }
}

fn compile_obj<'a>(compiler: &'a Compiler, obj: &'a Object) -> Result<FloatValue<'a>, String> {
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
        _ => println!("{}", obj),
    }

    compiler.fpm.run_on(&main_func);
    compiler.module.print_to_stderr();

    Ok(())
}
