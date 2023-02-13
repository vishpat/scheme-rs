use crate::env::*;
use crate::object::*;
use crate::parser::*;

fn eval_binary_op(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err(format!("Invalid number of arguments for infix operator"));
    }
    let operator = list[0].clone();
    let left = eval_obj(&list[1].clone(), env)?;
    let right = eval_obj(&list[2].clone(), env)?;
    let left_val = match left {
        Object::Integer(n) => n,
        _ => return Err(format!("Left operand must be an integer {:?}", left)),
    };
    let right_val = match right {
        Object::Integer(n) => n,
        _ => return Err(format!("Right operand must be an integer {:?}", right)),
    };
    match operator {
        Object::Symbol(s) => match s.as_str() {
            "+" => Ok(Object::Integer(left_val + right_val)),
            "-" => Ok(Object::Integer(left_val - right_val)),
            "*" => Ok(Object::Integer(left_val * right_val)),
            "/" => Ok(Object::Integer(left_val / right_val)),
            "<" => Ok(Object::Bool(left_val < right_val)),
            ">" => Ok(Object::Bool(left_val > right_val)),
            "=" => Ok(Object::Bool(left_val == right_val)),
            "!=" => Ok(Object::Bool(left_val != right_val)),
            _ => Err(format!("Invalid infix operator: {}", s)),
        },
        _ => Err(format!("Operator must be a symbol")),
    }
}

fn eval_define(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err(format!("Invalid number of arguments for define"));
    }

    let sym = match &list[1] {
        Object::Symbol(s) => s.clone(),
        _ => return Err(format!("Invalid define")),
    };
    let val = eval_obj(&list[2], env)?;
    env.set(&sym, val);
    Ok(Object::Void)
}

fn eval_if(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 4 {
        return Err(format!("Invalid number of arguments for if statement"));
    }

    let cond_obj = eval_obj(&list[1], env)?;
    let cond = match cond_obj {
        Object::Bool(b) => b,
        _ => return Err(format!("Condition must be a boolean")),
    };

    if cond == true {
        return eval_obj(&list[2], env);
    } else {
        return eval_obj(&list[3], env);
    }
}

fn eval_quote(list: &Vec<Object>) -> Result<Object, String> {
    if list.len() != 2 {
        return Err(format!("Invalid number of arguments for quote statement"));
    }

    return Ok(list[1].clone());
}

fn eval_function_definition(list: &Vec<Object>) -> Result<Object, String> {
    let params = match &list[1] {
        Object::List(list) => {
            let mut params = Vec::new();
            for param in list {
                match param {
                    Object::Symbol(s) => params.push(s.clone()),
                    _ => return Err(format!("Invalid lambda parameter")),
                }
            }
            params
        }
        _ => return Err(format!("Invalid lambda")),
    };

    let body = match &list[2] {
        Object::List(list) => list.clone(),
        _ => return Err(format!("Invalid lambda")),
    };
    Ok(Object::Lambda(params, body))
}

fn eval_function_call(s: &str, list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    let lamdba = env.get(s);
    if lamdba.is_none() {
        return Err(format!("Unbound symbol: {}", s));
    }

    let func = lamdba.unwrap();
    match func {
        Object::Lambda(params, body) => {
            let mut new_env = Env::extend(env);
            for (i, param) in params.iter().enumerate() {
                let val = eval_obj(&list[i + 1], &mut new_env)?;
                new_env.set(param, val);
            }
            return eval_obj(&Object::List(body), &mut new_env);
        }
        _ => return Err(format!("Not a lambda: {}", s)),
    }
}

fn eval_symbol(s: &str, env: &mut Env) -> Result<Object, String> {
    let val = match s {
        "#t" | "else" => return Ok(Object::Bool(true)),
        "#f" => return Ok(Object::Bool(false)),
        _ => env.get(s),
    };

    if val.is_none() {
        return Err(format!("Unbound symbol: {}", s));
    }
    Ok(val.unwrap().clone())
}

fn eval_display(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 2 {
        return Err(format!("Invalid number of arguments for display"));
    }

    let sym = eval_obj(&list[1].clone(), env)?;
    println!("{:?}", sym);
    Ok(Object::Void)
}

fn eval_cond(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() < 2 {
        return Err(format!("Invalid number of arguments for cond"));
    }

    for l in list[1..].iter() {
        match l {
            Object::List(list) => {
                if list.len() != 2 {
                    return Err(format!("Invalid cond clause {:?}", list));
                }
                let cond = eval_obj(&list[0], env)?;
                let cond_val = match cond {
                    Object::Bool(b) => b,
                    _ => return Err(format!("Condition must be a boolean {:?}", cond)),
                };
                if cond_val == true {
                    return eval_obj(&list[1], env);
                }
            }
            _ => return Err(format!("Invalid cond clause")),
        }
    }

    return Err(format!("No cond clause matched"));
}

fn eval_list(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    // Empty list
    if list.len() == 0 {
        return Ok(Object::Void);
    }

    let head = &list[0];
    match head {
        Object::Symbol(s) => match s.as_str() {
            "+" | "-" | "*" | "/" | "<" | ">" | "=" | "!=" => {
                return eval_binary_op(&list, env);
            }
            "quote" => eval_quote(&list),
            "define" => eval_define(&list, env),
            "if" => eval_if(&list, env),
            "lambda" => eval_function_definition(&list),
            "display" => eval_display(&list, env),
            "cond" => eval_cond(&list, env),
            _ => eval_function_call(&s, &list, env),
        },
        _ => Err(format!("Invalid list head {:?}", head)),
    }
}

fn eval_obj(obj: &Object, env: &mut Env) -> Result<Object, String> {
    match obj {
        Object::List(list) => eval_list(list, env),
        Object::Void => Ok(Object::Void),
        Object::Lambda(_params, _body) => Ok(Object::Void),
        Object::Bool(_) => Ok(obj.clone()),
        Object::Integer(n) => Ok(Object::Integer(*n)),
        Object::Symbol(s) => eval_symbol(s, env),
    }
}

pub fn eval(program: &str, env: &mut Env) -> Result<Object, String> {
    let parsed_list = parse(program);
    if parsed_list.is_err() {
        return Err(format!("{}", parsed_list.err().unwrap()));
    }

    match parsed_list.unwrap() {
        Object::List(list) => {
            let mut r = Object::Void;
            for l in list {
                let result = eval_obj(&l, env);
                if result.is_err() {
                    return Err(format!("{}", result.err().unwrap()));
                }
                r = result.unwrap();
            }
            Ok(r)
        }
        _ => return Err(format!("Invalid program")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_add() {
        let mut env = Box::new(Env::new());
        let result = eval("(+ 1 2)", &mut env).unwrap();
        assert_eq!(result, Object::Integer(3));
    }

    #[test]
    fn test_area_of_a_circle() {
        let mut env = Box::new(Env::new());
        let program = "
            (define r 10)
            (define pi 314)
            (* pi (* r r))
        ";
        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer((314 * 10 * 10) as i64));
    }

    #[test]
    fn test_sqr_function() {
        let mut env = Box::new(Env::new());
        let program = "
            (define sqr 
                (lambda (r) (* r r))) 
            (sqr 10)
        ";
        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer((10 * 10) as i64));
    }

    #[test]
    fn test_fibonaci() {
        let mut env = Box::new(Env::new());
        let program = "
            (define fib 
                (lambda (n) 
                    (if (< n 2) 
                    1 (+ (fib (- n 1)) (fib (- n 2))))))
            (fib 10)
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer((89) as i64));
    }

    #[test]
    fn test_factorial() {
        let mut env = Box::new(Env::new());
        let program = "
            (define fact 
                (lambda (n) 
                    (if (< n 1) 
                        1 
                        (* n (fact (- n 1))))))
            (fact 5)
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer((120) as i64));
    }

    #[test]
    fn test_circle_area_function() {
        let mut env = Box::new(Env::new());
        let program = "
            (define pi 314)
            (define r 10)
            (define sqr 
                (lambda (r) (* r r)))
            (define area 
                (lambda (r) (* pi (sqr r))))
            (area r)
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer((314 * 10 * 10) as i64));
    }

    #[test]
    fn test_quote_1() {
        let mut env = Box::new(Env::new());
        let program = "
            (quote (1 2 3))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3)
            ])
        );
    }

    #[test]
    fn test_quote_2() {
        let mut env = Box::new(Env::new());
        let program = "
            (quote a)
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Symbol("a".to_string()));
    }

    #[test]
    fn test_cond_1() {
        let mut env = Box::new(Env::new());
        let program = "
            (cond ((> 2 1) 5) 
                  ((< 2 1) 10) 
                  (else 15))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(5));
    }

    #[test]
    fn test_cond_2() {
        let mut env = Box::new(Env::new());
        let program = "
            (cond ((> 1 2) 5) 
                  ((< 1 2) 10) 
                  (else 15))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(10));
    }

    #[test]
    fn test_cond_3() {
        let mut env = Box::new(Env::new());
        let program = "
            (cond ((> 1 2) 5) 
                  ((< 1 0) 10) 
                  (else 15))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(15));
    }
}
