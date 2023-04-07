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
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::FloatValue;
use log::debug;

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

    Ok(func.as_any_value_enum())
}

pub fn compile_function_definition<'a>(
    compiler: &'a Compiler,
    func_proto: &'a Object,
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

    let entry = compiler.context.append_basic_block(
        func_proto.into_function_value(),
        "entry",
    );
    compiler.builder.position_at_end(entry);
    compiler.sym_tables.borrow_mut().push_new_sym_table();

    for p in
        func_proto.into_function_value().get_param_iter()
    {
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

    let val = match func_body {
        Object::List(l) => {
            compile_list(compiler, l)?.into_float_value()
        }
        Object::Number(n) => {
            compile_number(compiler, n)?.into_float_value()
        }
        Object::Symbol(s) => {
            process_symbol(compiler, s)?.into_float_value()
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

    compiler.sym_tables.borrow_mut().pop_sym_table();

    compiler.builder.position_at_end(current_bb);
    Ok(compiler.float_type.const_zero())
}

pub fn compile_function_call<'a>(
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
