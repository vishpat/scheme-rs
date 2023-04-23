use crate::compiler::CompileResult;
use crate::compiler::Compiler;
use inkwell::values::AnyValue;

pub fn compile_number<'a>(
  compiler: &'a Compiler,
  n: &'a f64,
) -> CompileResult<'a> {
  Ok(
    compiler.float_type.const_float(*n).as_any_value_enum(),
  )
}
