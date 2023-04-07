use crate::compiler::compile_obj;
use crate::compiler::define::compile_define;
use crate::compiler::function::compile_function_call;
use crate::compiler::number::compile_number;
use crate::compiler::symbol::process_symbol;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::object::*;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::FloatPredicate;
use log::debug;

pub fn compile_quote<'a>(
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

    match &list[1] {
        Object::List(l) => {
            let mut prev = None;
            for obj in l.iter().rev() {
                let ir_obj = compile_obj(compiler, obj)?;
                let node_ptr =
                    compiler.builder.build_alloca(
                        compiler.node_type,
                        "node",
                    );
                let data_ptr =
                  compiler.builder.build_struct_gep(
                      compiler.node_type,
                      node_ptr,
                      0,
                      "data",
                  ).map_err(|_e| "Unable to build data pointer for struct".to_string())?;
                compiler.builder.build_store(
                    data_ptr,
                    ir_obj.into_float_value(),
                );

                let next_ptr =
                  compiler.builder.build_struct_gep(
                      compiler.node_type,
                      node_ptr,
                      1,
                      "next",
                  ).map_err(|_e| "Unable to build next pointer for struct".to_string())?;

                if let Some(prev) = prev {
                    compiler
                        .builder
                        .build_store(next_ptr, prev);
                } else {
                    compiler.builder.build_store(
                        next_ptr,
                        compiler.node_null,
                    );
                }
                prev = Some(node_ptr);
            }
            Ok(prev.unwrap().as_any_value_enum())
        }
        _ => Err("Expected number".to_string()),
    }
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

pub fn compile_list<'a>(
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
