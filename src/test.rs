#[cfg(test)]
mod tests {
    use crate::env;
    use crate::eval;
    use crate::object::Object;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_compose() {
        let mut env = Rc::new(RefCell::new(env::Env::new()));
        let program = "
            (define compose 
                (lambda (f g x)
                    (f (g x))))

            (define even? 
                (lambda (n) 
                    (if (= (mod n 2) 0) 
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

    #[test]
    fn test_map_1() {
        let mut env = Rc::new(RefCell::new(env::Env::new()));
        let program = "
            (define map 
                (lambda (f a-list)
                (cond ((null? a-list) a-list)
                    (#t (cons (f (car a-list)) (map f (cdr a-list)))))))
            
            (define even? 
                (lambda (n) 
                    (if (= (mod n 2) 0) 
                        #t 
                        #f
                    )
                )
            ) 
            (map even? (quote (1 2 3 4)))
        ";
        let result = eval::eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![
                Object::Bool(false),
                Object::Bool(true),
                Object::Bool(false),
                Object::Bool(true)
            ])
        );
    }

    #[test]
    fn test_map_2() {
        let mut env = Rc::new(RefCell::new(env::Env::new()));
        let program = "
            (define map 
                (lambda (f a-list)
                (cond ((null? a-list) a-list)
                    (#t (cons (f (car a-list)) (map f (cdr a-list)))))))

            (define add-n 
                (lambda (x) 
                    (lambda (a) (+ x a))))
            (define add-5 (add-n 5))

            (map add-5 (quote (1 2 3 4)))
        ";
        let result = eval::eval(program, &mut env).unwrap();
        assert_eq!(
            result,
            Object::List(vec![
                Object::Integer(6),
                Object::Integer(7),
                Object::Integer(8),
                Object::Integer(9)
            ])
        );
    }

    #[test]
    fn test_foldr() {
        let mut env = Rc::new(RefCell::new(env::Env::new()));
        let program = "
            (define add 
                (lambda (x y) 
                    (+ x y)))

            (define foldr 
                (lambda (func end lst)
                    (if (null? lst)
                        end
                        (func (car lst) (foldr func end (cdr lst)))))) 
            
            (define sum 
                (lambda (lst) 
                    (foldr add 0 lst)))

            (sum (quote (1 2 3 4)))
        ";
        let result = eval::eval(program, &mut env).unwrap();
        assert_eq!(result, Object::Integer(10));
    }
}
