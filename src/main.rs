mod env;
mod eval;
mod lexer;
mod object;
mod parser;

use linefeed::{Interface, ReadResult};
use object::Object;

const PROMPT: &str = "lisp-rs> ";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let reader = Interface::new(PROMPT).unwrap();
    let mut env = Box::new(env::Env::new());

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
            Object::Lambda(params, body) => {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_even() {
        let mut env = Box::new(env::Env::new());
        let program = "
            (define compose 
                (lambda (f g x)
                    (f (g x))))

            (define even? 
                (lambda (n) 
                    (if (= (% n 2) 0) 
                        #t 
                        #f
                    )
                )
            ) 
            (compose even? (lambda (x) (- x 1)) 10)
        ";
        let result = eval::eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(false));
    }
}
