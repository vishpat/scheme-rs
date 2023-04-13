use crate::compiler::function::compile_function_definition;
use crate::compiler::list::compile_list;
use crate::compiler::number::compile_number;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::object::*;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum::{
    FloatValue, PointerValue,
};
use inkwell::AddressSpace;
use log::debug;

pub fn compile_define_obj<'a>(
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

    let val = match &list[2] {
        Object::Number(n) => compile_number(compiler, n),
        Object::List(l) => {
            println!("Processing list: {:?}", l);
            compile_list(compiler, l)
        }
        _ => {
            return Err(format!(
                "Expected number, found: {}",
                list[1]
            ))
        }
    }?;

    match val {
        FloatValue(f) => {
            let ptr = compiler
                .builder
                .build_alloca(compiler.float_type, name);
            compiler.builder.build_store(ptr, f);
            let global_val = compiler.module.add_global(
                compiler.float_type,
                Some(AddressSpace::default()),
                name,
            );
            global_val.set_initializer(&f);
        }
        PointerValue(p) => {
            let ptr = compiler
                .builder
                .build_alloca(p.get_type(), name);
            compiler.builder.build_store(ptr, p);

            let global_val = compiler.module.add_global(
                compiler
                    .node_type
                    .ptr_type(AddressSpace::default()),
                Some(AddressSpace::default()),
                name,
            );
            global_val.set_initializer(&p);
        }
        _ => {
            return Err(format!(
                "Unexpected value: {:?}",
                list[1]
            ))
        }
    }

    Ok(val.as_any_value_enum())
}

pub fn compile_define<'a>(
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
    match &list[1] {
        Object::Symbol(_) => {
            compile_define_obj(compiler, list)
        }
        Object::List(_) => {
            compile_function_definition(
                compiler, &list[1], &list[2],
            )?;
            Ok(compiler
                .float_type
                .const_zero()
                .as_any_value_enum())
        }
        _ => Err("Expected symbol".to_string()),
    }
}