use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use crate::sym_table::*;
use inkwell::values::AnyValue;
use inkwell::AddressSpace;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

pub fn process_symbol<'ctx>(
    compiler: &'ctx Compiler,
    sym: &str,
    sym_tables: &mut Rc<RefCell<SymTables<'ctx>>>,
) -> CompileResult<'ctx> {
    let val = sym_tables.borrow().get_symbol_value(sym);

    debug!("Processing symbol {} val: {:?}", sym, val);
    let x = match val {
        Some(p) => {
            if p.data_type == DataType::Number {
                debug!(
                    "Loading Number symbol: {} {:?}",
                    sym, p
                );
                compiler.builder.build_load(
                    compiler.float_type,
                    p.ptr,
                    sym,
                )
            } else if p.data_type == DataType::List {
                debug!(
                    "Loading List symbol: {} {:?}",
                    sym, p
                );
                compiler.builder.build_load(
                    compiler
                        .node_type
                        .ptr_type(AddressSpace::default()),
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

    debug!(
        "Returning after processing symbol {} val: {:?}",
        sym, x
    );

    Ok(x.as_any_value_enum())
}
