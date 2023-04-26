(define (add x y) 
       (+ x y))

(define (foldr func:f2 end lst:l)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

(define (sum lst:l) 
        (foldr add 0 lst))


<<<<<<< HEAD
(define (map:l proc:f1 lst:l)
=======
(define (map proc:f1 lst:l)
>>>>>>> 30fffd371c386875bbbaa8ff98d7f83caf0e7ceb
  (if (null? lst)
      (quote ()) 
      (cons (proc (car lst))
            (map proc (cdr lst)))))

(define (add1 x)
  (+ x 1))

(define lst (quote (1 2 3 4 5)))

(car (map add1 lst))