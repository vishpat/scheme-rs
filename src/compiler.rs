use crate::object::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{FloatValue, FunctionValue};
use inkwell::FloatPredicate;

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
            "+" | "-" | "*" | "/" => {
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

                        compiler
                            .builder
                            .build_unsigned_int_to_float(
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

pub fn compile<'a>(compiler: &'a Compiler, obj: &'a Object) -> Result<FloatValue<'a>, String> {
    let val = match obj {
        Object::Number(n) => compile_number(compiler, n),
        Object::List(list) => compile_list(compiler, list),
        _ => Err(format!("Cannot compile object: {:?}", obj)),
    };
    val
}
