mod env;
mod eval;
mod lexer;
mod object;
mod parser;
mod test;

use linefeed::{Interface, ReadResult};
use object::Object;
use std::cell::RefCell;
use std::rc::Rc;
use std::env::args;
use std::fs::File;
use std::io::Read;


const PROMPT: &str = "lisp-rs> ";

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let reader = Interface::new(PROMPT).unwrap();
    let mut env = Rc::new(RefCell::new(env::Env::new()));

    reader.set_prompt(PROMPT.to_string().as_ref()).unwrap();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        }
        let val = eval::eval(input.as_ref(), &mut env)?;
        match val {
            Object::Void => {}
            Object::Integer(n) => println!("{}", n),
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
    let args: Vec<String> = args().collect();

    if args.len() < 2 {
        return repl() 
    } else {
        let mut file = File::open(&args[1]).expect("File not found");
        let mut contents = String::new();
        file.read_to_string(&mut contents).expect("Could not read file");
        let mut env = Rc::new(RefCell::new(env::Env::new()));
        let result = eval::eval(&contents, &mut env).unwrap();
        println!("{}", result);
    }   

    Ok(()) 
}
