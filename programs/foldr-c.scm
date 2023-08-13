(define (add x y) 
       (+ x y))

(define (foldr func:f2 end lst:l)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

(define (sum lst:l) 
    (foldr add 0 lst))

(print  (sum (quote (1 2 3 4 5)))) 
