use std::collections::HashMap;
use crate::object::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{AnyValue, FunctionValue, PointerValue};

pub struct Compiler<'a, 'ctx> {
  context: &'ctx Context,
  builder: &'a Builder<'ctx>,
  fpm: &'a PassManager<FunctionValue<'ctx>>,
  module: &'a Module<'ctx>,

  variables: HashMap<String, PointerValue<'ctx>>,
  fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {

  pub fn new(
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    fpm: &'a PassManager<FunctionValue<'ctx>>,
    module: &'a Module<'ctx>,
  ) -> Self {
    Self {
      context,
      builder,
      fpm,
      module,
      variables: HashMap::new(),
      fn_value_opt: None,
    }
  }

  fn compile_number(&self, n: &f64) -> Result<(), String> {
    let value = self.context.f64_type().const_float(*n);
    self.builder.build_return(Some(&value));
    Ok(())
  }

  fn compile_call(&self, list: &Vec<Object>) -> Result<(), String> {
    let mut args = Vec::new();
    for obj in list {
      match obj {
        Object::Number(n) => {
          let value = self.context.f64_type().const_float(*n);
          args.push(value.as_basic_value_enum());
        }
        Object::Symbol(s) => {
          let value = self.variables.get(s).unwrap();
          let value = self.builder.build_load(*value, s);
          args.push(value.as_basic_value_enum());
        }
        _ => return Err(format!("Cannot compile object: {:?}", obj)),
      }
    }

    let fn_value = self.fn_value_opt.unwrap();
    let value = self.builder.build_call(fn_value, &args, "calltmp");
    let value = self.builder.build_return(Some(&value.try_as_basic_value().left().unwrap()));
    Ok(())
  }

  fn compile_list(&self, list: &Vec<Object>) -> Result<(), String> {
    if list.is_empty() {
      return Err("Cannot compile empty list".to_string());
    }

    match &list[0] {
      Object::Symbol(s) => {
        match s.as_str() {
          "+" | "-" | "*" | "/" => {
            if list.len() != 3 {
              return Err(format!("Expected 2 arguments, found {}", list.len() - 1));
            }
            let left = self.compile(&list[1]);
            let right = self.compile(&list[2]);
            match (left, right) {
              (Object::Number(l), Object::Number(r)) => {
                let value = match s.as_str() {
                  "+" => self.context.f64_type().const_float(l + r),
                  "-" => self.context.f64_type().const_float(l - r),
                  "*" => self.context.f64_type().const_float(l * r),
                  "/" => self.context.f64_type().const_float(l / r),
                  _ => unreachable!(),
                };
                self.builder.build_return(Some(&value));
              }
              _ => return Err(format!("Cannot compile object: {:?}", list)),
            }
            self.compile_call(list)
          },
          _ => self.compile_call(list),
        }
      }
      _ => self.compile_call(list),
    }
  }

  pub fn compile(self, obj: &Object) -> Result<(), String> {
    match obj {
      Object::Number(n) => self.compile_number(n),
      Object::List(list) => self.compile_list(list),
      _ => Err(format!("Cannot compile object: {:?}", obj)),
    }
  }
}