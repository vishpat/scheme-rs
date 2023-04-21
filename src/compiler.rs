mod define;
mod function;
mod list;
mod number;
mod symbol;
use crate::compiler::list::compile_list;
use crate::compiler::number::compile_number;
use crate::compiler::symbol::process_symbol;
use crate::object::*;
use crate::parser::*;
use crate::sym_table::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use log::debug;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

const MAIN_FUNC_NAME: &str = "main";

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub fpm:
        inkwell::passes::PassManager<FunctionValue<'ctx>>,
    pub int_type: inkwell::types::IntType<'ctx>,
    pub float_type: inkwell::types::FloatType<'ctx>,
    pub node_type: inkwell::types::StructType<'ctx>,
    pub node_null: inkwell::values::PointerValue<'ctx>,
    pub func1_obj_type: inkwell::types::StructType<'ctx>,
    pub func1_ptr_type: inkwell::types::BasicTypeEnum<'ctx>,
    pub func2_obj_type: inkwell::types::StructType<'ctx>,
    pub func2_ptr_type: inkwell::types::BasicTypeEnum<'ctx>,
    pub bool_type: inkwell::types::IntType<'ctx>,
    pub main_func: FunctionValue<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(MAIN_FUNC_NAME);
        let fpm = PassManager::create(&module);
        let node_type = context.opaque_struct_type("node");
        node_type.set_body(
            &[
                context.f64_type().into(),
                node_type
                    .ptr_type(AddressSpace::default())
                    .into(),
            ],
            false,
        );
        let node_null = builder.build_int_to_ptr(
            context.i64_type().const_zero(),
            node_type.ptr_type(AddressSpace::default()),
            "null",
        );
        let func1_obj_type =
            context.opaque_struct_type("func1_obj");
        let func1_ptr_type = context
            .f64_type()
            .fn_type(&[context.f64_type().into()], false)
            .ptr_type(AddressSpace::default())
            .into();
        func1_obj_type.set_body(&[func1_ptr_type], false);

        let func2_obj_type =
            context.opaque_struct_type("func2_obj");
        let func2_ptr_type = context
            .f64_type()
            .fn_type(
                &[
                    context.f64_type().into(),
                    context.f64_type().into(),
                ],
                false,
            )
            .ptr_type(AddressSpace::default())
            .into();
        func2_obj_type.set_body(&[func2_ptr_type], false);

        let bool_type = context.bool_type();
        let main_func = module.add_function(
            MAIN_FUNC_NAME,
            context.i64_type().fn_type(&[], false),
            None,
        );

        Self {
            context,
            builder,
            module,
            fpm,
            int_type: context.i64_type(),
            float_type: context.f64_type(),
            node_type,
            node_null,
            func1_obj_type,
            func1_ptr_type,
            func2_obj_type,
            func2_ptr_type,
            bool_type,
            main_func,
        }
    }
}

type CompileResult<'ctx> =
    Result<AnyValueEnum<'ctx>, String>;

pub fn compile_and_run_program(
    program: &str,
) -> Result<i32, String> {
    let obj = parse(program).unwrap_or_else(|e| {
        panic!("Error parsing program: {}", e)
    });
    let context = Context::create();
    let compiler = Compiler::new(&context);
    let mut sym_tables =
        Rc::new(RefCell::new(SymTables::new()));
    let mut obj_vec = vec![];
    let main_func = compiler.main_func;

    let main_block = compiler
        .context
        .append_basic_block(main_func, "entry");
    compiler.builder.position_at_end(main_block);

    match obj {
        Object::List(list) => {
            let list_len = list.len();
            let mut idx = 0;
            for obj in list {
                obj_vec.push(obj);
            }

            while idx < list_len {
                let val = compile_obj(
                    &compiler,
                    &obj_vec[idx],
                    &mut sym_tables,
                )?;
                let ret_val = match val {
                    AnyValueEnum::FloatValue(_) => {
                        let val = val.into_float_value();
                        compiler
                            .builder
                            .build_float_to_unsigned_int(
                                val,
                                compiler.int_type,
                                "rettmp",
                            )
                    }
                    _ => compiler.int_type.const_zero(),
                };

                idx += 1;
                if idx == list_len {
                    compiler
                        .builder
                        .build_return(Some(&ret_val));
                }
            }
        }
        _ => debug!("{}", obj),
    }

    compiler.fpm.run_on(&main_func);
    compiler.module.print_to_stderr();
    compiler
        .module
        .print_to_file(Path::new("main.ll"))
        .unwrap();

    println!("Running execution engine...");
    let execution_engine = compiler
        .module
        .create_jit_execution_engine(
            OptimizationLevel::None,
        )
        .unwrap();

    let main = execution_engine
        .get_function_value(MAIN_FUNC_NAME)
        .unwrap();

    let ret = unsafe {
        execution_engine.run_function_as_main(main, &[])
    };

    println!("Result: {:?}", ret);
    Ok(ret)
}

fn compile_obj<'a>(
    compiler: &'a Compiler,
    obj: &'a Object,
    sym_tables: &mut Rc<RefCell<SymTables<'a>>>,
) -> CompileResult<'a> {
    debug!("Compiling Object: {:?}", obj);
    let val = match obj {
        Object::Number(n) => compile_number(compiler, n),
        Object::List(list) => {
            compile_list(compiler, list, sym_tables)
        }
        Object::Symbol(s) => {
            let val =
                process_symbol(compiler, s, sym_tables)?;
            match val {
                AnyValueEnum::FloatValue(v) => {
                    Ok(v.as_any_value_enum())
                }
                AnyValueEnum::PointerValue(v) => {
                    Ok(v.as_any_value_enum())
                }
                AnyValueEnum::FunctionValue(v) => {
                    Ok(v.as_any_value_enum())
                }
                _ => Err(format!(
                    "Cannot compile object for symbol: {:?}",
                    obj
                )),
            }
        }
        _ => {
            Err(format!("Cannot compile object: {:?}", obj))
        }
    };
    val
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_add() {
        let program = "(+ 1 2)";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 3);
    }

    #[test]
    fn test_simple_sub() {
        let program = "(- (+ 1 2) (- 3 4))";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 4);
    }

    #[test]
    fn test_fibonaci() {
        let program = "
        (define (fib n)
            (if (<= n 2)
                1
                (+ (fib (- n 1)) (fib (- n 2)))))
        (fib 10)";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 55);
    }

    #[test]
    fn test_simple_if() {
        let program = "(if (> 1 2) 1 2)";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 2);
    }

    #[test]
    fn test_functions() {
        let program = "
            (define pi 3.14)

            (define (area-of-circle r)
                    (* pi (* r r)))

            (define (area-of-square x)
                    (* x x))

            (area-of-circle 4)
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 50);
    }

    #[test]
    fn test_define() {
        let program = "
            (define pi 3.14)
            pi
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 3);
    }

    #[test]
    fn test_functions_2() {
        let program = "
            (define (pi) 3.14)
            (pi)
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 3);
    }

    #[test]
    fn test_quote() {
        let program = "
            (quote (1 2 3))
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 0);
    }

    #[test]
    fn test_quote_empty_list() {
        let program = "
            (quote ())
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 0);
    }

    #[test]
    fn test_null() {
        let program = "
            (if (null? (quote ())) 1 2)
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 1);
    }

    #[test]
    fn test_not_null() {
        let program = "
            (if (null? (quote (1))) 1 2)
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 2);
    }

    #[test]
    fn test_car() {
        let program = "
            (car (quote (2 3 4)))
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 2);
    }

    #[test]
    fn test_cdr() {
        let program = "
            (car (cdr (quote (2 3 4))))
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 3);
    }

    #[test]
    fn test_list_parameter() {
        let program = "
        (define x (quote (10 2 3)))
        (define (first l_list)
                  (car l_list))

        (first x)
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 10);
    }

    #[test]
    fn test_recursive_sum() {
        let program = "
        (define data (quote (1 2 3 4 5)))
        (define (sum l_list)
            (if (null? l_list)
                0
                (+ (car l_list) (sum (cdr l_list)))))
        (sum data)
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 15);
    }

    #[test]
    fn test_recursive_sum_2() {
        let program = "
        (define (add x y) 
            (+ x y))

        (define (foldr f2_func end l_lst)
                (if (null? l_lst)
                    end
                    (f2_func (car l_lst) (foldr f2_func end (cdr l_lst))))) 

        (define (sum l_lst) 
            (foldr add 0 l_lst))

        (sum (quote (1 2 3 4 5))) 
        ";
        let ret = compile_and_run_program(program).unwrap();
        assert_eq!(ret, 15);
    }
}
