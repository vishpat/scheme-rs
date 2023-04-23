(define (map f1_proc l_lst)
  (if (null? l_lst)
      (quote ()) 
      (cons (f1_proc (car l_lst))
            (map f1_proc (cdr l_lst)))))

(define (add1 x)
  (+ x 1))

(define lst (quote (1 2 3 4 5)))

(car (map add1 lst))