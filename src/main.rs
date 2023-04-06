mod compiler;
mod env;
mod eval;
mod lexer;
mod object;
mod parser;
mod sym_table;
mod test;

use compiler::compile_and_run_program;
use linefeed::{Interface, ReadResult};
use object::Object;
use std::cell::RefCell;
use std::env::args;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

extern crate env_logger;

const PROMPT: &str = "lisp-rs> ";

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let reader = Interface::new(PROMPT).unwrap();
    let mut env = Rc::new(RefCell::new(env::Env::new()));

    reader.set_prompt(PROMPT.to_string().as_ref()).unwrap();

    while let ReadResult::Input(input) =
        reader.read_line().unwrap()
    {
        if input.eq("exit") {
            break;
        }
        let val = eval::eval(input.as_ref(), &mut env)?;
        match val {
            Object::Void => {}
            Object::Number(n) => println!("{}", n),
            Object::Bool(b) => println!("{}", b),
            Object::Symbol(s) => println!("{}", s),
            Object::Lambda(params, body, _) => {
                println!("Lambda(");
                for param in params {
                    println!("{} ", param);
                }
                println!(")");
                for expr in body {
                    println!(" {}", expr);
                }
            }
            _ => println!("{}", val),
        }
    }

    println!("Good bye");
    Ok(())
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let args: Vec<String> = args().collect();

    if args.len() < 2 {
        return repl();
    } else if args[1] == "-i" {
        let mut file =
            File::open(&args[2]).expect("File not found");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Could not read file");
        let mut env =
            Rc::new(RefCell::new(env::Env::new()));
        let result =
            eval::eval(&contents, &mut env).unwrap();
        println!("{}", result);
    } else if args[1] == "-c" {
        let mut file =
            File::open(&args[2]).expect("File not found");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Could not read file");
        compile_and_run_program(&contents)
            .unwrap_or_else(|e| panic!("{}", e));
    }

    Ok(())
}
