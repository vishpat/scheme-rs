#[cfg(test)]
mod tests {
  use crate::compiler::compile_and_run_program;

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
        (define (first list:l)
                  (car list))

        (first x)
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 10);
  }

  #[test]
  fn test_recursive_sum() {
    let program = "
        (define data (quote (1 2 3 4 5)))
        (define (sum list:l)
            (if (null? list)
                0
                (+ (car list) (sum (cdr list)))))
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

        (define (foldr func:f2 end lst:l)
                (if (null? lst)
                    end
                    (func (car lst) (foldr func end (cdr lst))))) 

        (define (sum lst:l) 
            (foldr add 0 lst))

        (sum (quote (1 2 3 4 5))) 
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 15);
  }

  #[test]
  fn test_true() {
    let program = "
            (if #t 1 2)
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 1);
  }

  #[test]
  fn test_false() {
    let program = "
            (if #f 1 2)
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 2);
  }

  #[test]
  fn test_cons_1() {
    let program = "
    (define (add x y) 
        (+ x y))

    (define (foldr func:f2 end lst:l)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

    (define (sum lst:l) 
        (foldr add 0 lst))
 
    (sum (cons 1 (cons 2 (cons 3 (quote ())))))
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 6);
  }

  #[test]
  fn test_cons_2() {
    let program = "
    (define (add x y) 
        (+ x y))

    (define (foldr func:f2 end lst:l)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

    (define (sum lst:l) 
        (foldr add 0 lst))
 
    (sum (cons 4 (quote (5 6))))
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 15);
  }

  #[test]
  fn test_cons_3() {
    let program = "
            (car (cons 4 (quote ())))
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 4);
  }

  #[test]
  fn test_cons_5() {
    let program = "
      (define (add x y) 
          (+ x y))

      (define (foldr func:f2 end lst:l)
          (if (null? lst)
              end
              (func (car lst) (foldr func end (cdr lst))))) 

      (define (sum lst:l) 
          (foldr add 0 lst))
      (sum (cons 4 (quote (5 6))))
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 15);
  }

  #[test]
  fn test_cons_6() {
    let program = "
      (define (add x y) 
          (+ x y))

      (define (foldr func:f2 end lst:l)
          (if (null? lst)
              end
              (func (car lst) (foldr func end (cdr lst))))) 

      (define (sum lst:l) 
          (foldr add 0 lst))
      (sum (cons 4 5))
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 9);
  }

  #[test]
  fn test_car_cdr() {
    let program = "
            (car (cdr (cdr (quote (1 2 3)))))   
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 3);
  }

  #[test]
  fn test_map() {
    let program = "
    (define (add x y) 
      (+ x y))

    (define (foldr func:f2 end lst:l)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

    (define (sum lst:l) 
        (foldr add 0 lst))

    (define (map:l proc:f1 lst:l)
        (if (null? lst)
            (quote ()) 
            (cons (proc (car lst))
                  (map proc (cdr lst)))))

    (define (add1 x)
        (+ x 1))

    (define lst (quote (1 2 3 4 5)))

    (sum (cdr (map add1 lst)))
        ";
    let ret = compile_and_run_program(program).unwrap();
    assert_eq!(ret, 18);
  }
}
