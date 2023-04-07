use crate::compiler::function::compile_function_definition;
use crate::compiler::list::compile_list;
use crate::compiler::number::compile_number;
use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::object::*;
use crate::sym_table::*;
use inkwell::values::AnyValue;
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
