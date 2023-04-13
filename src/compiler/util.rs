use crate::compiler::Compiler;
use inkwell::{values::{FunctionValue, PointerValue}, types::BasicType};

pub fn create_entry_block_alloca<'ctx, T>(
    compiler: &'ctx Compiler,
    function: &'ctx FunctionValue,
    typ: T,
    sym: &str,
) -> PointerValue<'ctx>
where T: BasicType<'ctx> {
    let entry = function.get_first_basic_block().unwrap();

    match entry.get_first_instruction() {
        Some(first_instr) => {
            compiler.builder.position_before(&first_instr)
        }
        None => compiler.builder.position_at_end(entry),
    }

    compiler.builder.build_alloca(typ, sym)
}
