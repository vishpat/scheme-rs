; Add 1 to each element of a list and then sum it

(define (add x y) 
       (+ x y))

; Reduce function for lists
(define (foldr func:f2 end lst:l)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

; Map function for lists
(define (map:l proc:f1 lst:l)
  (if (null? lst)
      (quote ()) 
      (cons (proc (car lst))
            (map proc (cdr lst)))))

(define (add1 x)
  (+ x 1))

(define lstx (quote (1 2 3 4 5)))

(print (foldr add 0 (map add1 lstx)))