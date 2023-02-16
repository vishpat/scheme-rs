use crate::env::*;
use crate::object::*;
use crate::parser::*;

enum LogicalOp {
    And,
    Or,
}

fn eval_binary_op(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err("Invalid number of arguments for infix operator".to_string());
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
            "%" => Ok(Object::Integer(left_val % right_val)),
            "<" => Ok(Object::Bool(left_val < right_val)),
            ">" => Ok(Object::Bool(left_val > right_val)),
            "=" => Ok(Object::Bool(left_val == right_val)),
            "!=" => Ok(Object::Bool(left_val != right_val)),
            _ => Err(format!("Invalid infix operator: {}", s)),
        },
        _ => Err(format!("Operator must be a symbol {:?}", operator)),
    }
}

fn eval_cons(list: &[Object], env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err("Invalid number of arguments for cons".to_string());
    }

    let car = eval_obj(&list[1], env)?;
    let cdr = eval_obj(&list[2], env)?;
    let cdr_list = match cdr {
        Object::List(l) => l,
        _ => return Err("cdr must be a list".to_string()),
    };
    let mut new_list = vec![car];
    new_list.extend(cdr_list);
    Ok(Object::List(new_list))
}

fn eval_car(list: &[Object], env: &mut Env) -> Result<Object, String> {
    if list.len() != 2 {
        return Err("Invalid number of arguments for car".to_string());
    }

    let obj = eval_obj(&list[1], env)?;
    let list = match obj {
        Object::List(l) => l,
        _ => return Err("car must be a list".to_string()),
    };
    if list.is_empty() {
        return Err("car of empty list".to_string());
    }
    Ok(list[0].clone())
}

fn eval_cdr(list: &[Object], env: &mut Env) -> Result<Object, String> {
    if list.len() != 2 {
        return Err("Invalid number of arguments for cdr".to_string());
    }

    let obj = eval_obj(&list[1], env)?;
    let list = match obj {
        Object::List(l) => l,
        _ => return Err("cdr must be a list".to_string()),
    };
    if list.is_empty() {
        return Err("cdr of empty list".to_string());
    }
    Ok(Object::List(list[1..].to_vec()))
}

fn eval_define(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err("Invalid number of arguments for define".to_string());
    }

    let sym = match &list[1] {
        Object::Symbol(s) => s.clone(),
        _ => return Err("Invalid define".to_string()),
    };
    let val = eval_obj(&list[2], env)?;
    env.set(&sym, val);
    Ok(Object::Void)
}

fn eval_set(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err("Invalid number of arguments for set".to_string());
    }

    let sym = match &list[1] {
        Object::Symbol(s) => s.clone(),
        _ => return Err("Invalid define".to_string()),
    };
    let val = eval_obj(&list[2], env)?;
    env.set_existing(&sym, val)?;
    Ok(Object::Void)
}

fn eval_if(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 4 {
        return Err("Invalid number of arguments for if statement".to_string());
    }

    let cond_obj = eval_obj(&list[1], env)?;
    let cond = match cond_obj {
        Object::Bool(b) => b,
        _ => return Err("Condition must be a boolean".to_string()),
    };

    if cond {
        eval_obj(&list[2], env)
    } else {
        eval_obj(&list[3], env)
    }
}

fn eval_quote(list: &Vec<Object>) -> Result<Object, String> {
    if list.len() != 2 {
        return Err("Invalid number of arguments for quote statement".to_string());
    }

    Ok(list[1].clone())
}

fn eval_list_keyword(list: &[Object], env: &mut Env) -> Result<Object, String> {
    let mut new_list = Vec::new();

    for obj in list[1..].iter() {
        new_list.push(eval_obj(obj, env)?);
    }
    Ok(Object::List(new_list))
}

fn eval_function_definition(list: &[Object]) -> Result<Object, String> {
    let params = match &list[1] {
        Object::List(list) => {
            let mut params = Vec::new();
            for param in list {
                match param {
                    Object::Symbol(s) => params.push(s.clone()),
                    _ => return Err(format!("Invalid lambda parameter {:?}", param)),
                }
            }
            params
        }
        _ => return Err(format!("Invalid lambda {:?}", list[1].clone())),
    };

    let body = match &list[2] {
        Object::List(list) => list.clone(),
        _ => return Err(format!("Invalid lambda {:?}", list[2].clone())),
    };
    Ok(Object::Lambda(params, body))
}

fn eval_let(list: &[Object], env: &mut Env) -> Result<Object, String> {
    let mut result = Object::Void;
    let mut bindings_env = Env::new();

    if list.len() < 3 {
        return Err("Invalid number of arguments for let".to_string());
    }

    let bindings = match list[1].clone() {
        Object::List(bindings) => bindings,
        _ => return Err("Invalid bindings for let".to_string()),
    };

    for binding in bindings.iter() {
        let binding = match binding {
            Object::List(binding) => binding,
            _ => return Err("Invalid binding for let".to_string()),
        };

        if binding.len() != 2 {
            return Err("Invalid binding for let".to_string());
        }

        let name = match binding[0].clone() {
            Object::Symbol(name) => name,
            _ => return Err("Invalid binding for let".to_string()),
        };

        let value = eval_obj(&binding[1], env)?;
        bindings_env.set(name.as_str(), value);
    }

    let mut new_env = Env::extend(env);
    new_env.update(bindings_env);

    for obj in list[2..].iter() {
        result = eval_obj(obj, &mut new_env)?;
    }
    Ok(result)
}

fn eval_function_call(s: &str, list: &[Object], env: &mut Env) -> Result<Object, String> {
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
            eval_obj(&Object::List(body), &mut new_env)
        }
        _ => Err(format!("Not a lambda: {}", s)),
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
    Ok(val.unwrap())
}

fn eval_display(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 2 {
        return Err("Invalid number of arguments for display".to_string());
    }

    let sym = eval_obj(&list[1].clone(), env)?;
    println!("{:?}", sym);
    Ok(Object::Void)
}

fn eval_logical_operation(
    op: LogicalOp,
    list: &Vec<Object>,
    env: &mut Env,
) -> Result<Object, String> {
    if list.len() < 3 {
        return Err("Invalid number of arguments for logical operation".to_string());
    }

    let mut result = match op {
        LogicalOp::And => true,
        LogicalOp::Or => false,
    };
    for l in list[1..].iter() {
        let obj = eval_obj(l, env)?;
        let val = match obj {
            Object::Bool(b) => b,
            _ => return Err(format!("Invalid logical operation argument: {:?}", obj)),
        };
        result = match op {
            LogicalOp::And => result && val,
            LogicalOp::Or => result || val,
        };
    }
    Ok(Object::Bool(result))
}

fn eval_begin(list: &[Object], env: &mut Env) -> Result<Object, String> {
    let mut result = Object::Void;
    for obj in list[1..].iter() {
        result = eval_obj(obj, env)?;
    }
    Ok(result)
}

fn eval_equal(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() != 3 {
        return Err("Invalid number of arguments for eq?".to_string());
    }

    let obj1 = eval_obj(&list[1], env)?;
    let obj2 = eval_obj(&list[2], env)?;
    Ok(Object::Bool(obj1 == obj2))
}

fn eval_cond(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    if list.len() < 2 {
        return Err("Invalid number of arguments for cond".to_string());
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
                if cond_val {
                    return eval_obj(&list[1], env);
                }
            }
            _ => return Err("Invalid cond clause".to_string()),
        }
    }

    Err("No cond clause matched".to_string())
}

fn eval_obj_keyword(obj: &[Object], env: &mut Env) -> Result<Object, String> {
    if obj.len() != 2 {
        return Err("Invalid number of arguments for eval".to_string());
    }

    let parameter = eval_obj(&obj[1], env)?;
    eval_obj(&parameter, env)
}

fn eval_list(list: &Vec<Object>, env: &mut Env) -> Result<Object, String> {
    // Empty list
    if list.is_empty() {
        return Ok(Object::Void);
    }

    let head = &list[0];
    match head {
        Object::Symbol(s) => match s.as_str() {
            "+" | "-" | "*" | "/" | "<" | ">" | "=" | "!=" | "%" => eval_binary_op(list, env),
            "and" => eval_logical_operation(LogicalOp::And, list, env),
            "or" => eval_logical_operation(LogicalOp::Or, list, env),
            "quote" => eval_quote(list),
            "cons" => eval_cons(list, env),
            "car" => eval_car(list, env),
            "cdr" => eval_cdr(list, env),
            "list" => eval_list_keyword(list, env),
            "define" => eval_define(list, env),
            "begin" => eval_begin(list, env),
            "if" => eval_if(list, env),
            "eq?" => eval_equal(list, env),
            "set!" => eval_set(list, env),
            "lambda" => eval_function_definition(list),
            "display" => eval_display(list, env),
            "cond" => eval_cond(list, env),
            "let" => eval_let(list, env),
            "eval" => eval_obj_keyword(list, env),
            _ => eval_function_call(s, list, env),
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
        Object::String(s) => Ok(Object::String(s.clone())),
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
                    return Err(result.err().unwrap());
                }
                r = result.unwrap();
            }
            Ok(r)
        }
        _ => Err("Invalid program".to_string()),
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

    #[test]
    fn test_and_1() {
        let mut env = Box::new(Env::new());
        let program = "
            (and (> 1 2) 
                 (< 1 0))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(false));
    }

    #[test]
    fn test_and_2() {
        let mut env = Box::new(Env::new());
        let program = "
            (and (< 1 2) 
                 (< 2 3)
                 (> 3 0))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(true));
    }

    #[test]
    fn test_or_1() {
        let mut env = Box::new(Env::new());
        let program = "
            (or (> 1 2) 
                (< 1 0))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(false));
    }

    #[test]
    fn test_or_2() {
        let mut env = Box::new(Env::new());
        let program = "
            (or (< 1 2) 
                (< 2 3)
                (> 0 3))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(true));
    }

    #[test]
    fn test_let_1() {
        let mut env = Env::new();
        let program = "
            (let ((a 10) (b 20))
                (list a b)
            )
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![Object::Integer(10), Object::Integer(20),])
        );
    }

    #[test]
    fn test_let_2() {
        let mut env = Env::new();
        let program = "
            (define a 100)
            (let ((a 10) (b 20))
                (list a b)
            )
            (eval a)
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(100));
    }

    #[test]
    fn test_let_3() {
        let mut env = Env::new();
        let program = "
            (let ((x 2) (y 3))
                (let ((x 7)
                      (z (+ x y)))
                    (* z x))) 
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(35));
    }

    #[test]
    fn test_eq_1() {
        let mut env = Env::new();
        let program = "
            (eq? 1 1)
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(true));
    }

    #[test]
    fn test_eq_2() {
        let mut env = Env::new();
        let program = "
            (eq? (quote (1 2 3)) 
                 (quote (1 2 3))
            )
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(true));
    }

    #[test]
    fn test_eq_3() {
        let mut env = Env::new();
        let program = "
            (eq? \"xyz abc\" 
                 \"xyz abcd\")
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(false));
    }

    #[test]
    fn test_eq_4() {
        let mut env = Env::new();
        let program = "
            (eq? (list \"xyz abc\" \"abc\") 
                 (list \"xyz abc\" \"abc\"))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Bool(true));
    }

    #[test]
    fn test_set_1() {
        let mut env = Env::new();
        let program = "
            (define x 10)
            (set! x 20)
            x
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(20));
    }

    #[test]
    fn test_eval() {
        let mut env = Env::new();
        let program = "
            (eval 
                (quote 
                    (begin
                        (define fact 
                            (lambda (n) 
                                (if (< n 1) 
                                    1 
                                    (* n (fact (- n 1))))))
                    (fact 5)
                    )
                )
            )
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(120));
    }
    #[test]
    fn test_cons_1() {
        let mut env = Env::new();
        let program = "
            (cons 1 (cons 2 (cons 3 (cons 4 (quote ())))))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4)
            ])
        );
    }

    #[test]
    fn test_cons_2() {
        let mut env = Env::new();
        let program = "
            (cons (quote (a b c)) (quote (d))) 
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![
                Object::List(vec![
                    Object::Symbol("a".to_string()),
                    Object::Symbol("b".to_string()),
                    Object::Symbol("c".to_string())
                ]),
                Object::Symbol("d".to_string())
            ])
        );
    }

    #[test]
    fn test_cdr_1() {
        let mut env = Env::new();
        let program = "
            (cdr (quote ((a) b c d))))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![
                Object::Symbol("b".to_string()),
                Object::Symbol("c".to_string()),
                Object::Symbol("d".to_string())
            ])
        );
    }

    #[test]
    fn test_car_1() {
        let mut env = Env::new();
        let program = "
            (car (quote ((a) b c d))))
        ";

        let result = eval(program, &mut env).unwrap();
        assert_eq!(result, Object::List(vec![Object::Symbol("a".to_string()),]));
    }
}
