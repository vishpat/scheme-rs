use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::sym_table::*;
use inkwell::values::AnyValue;
use log::debug;

pub fn process_symbol<'ctx>(
    compiler: &'ctx Compiler,
    sym: &str,
) -> CompileResult<'ctx> {
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
