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

(define lstx (quote (1 2 3 4 5)))

(car (map add1 lstx))