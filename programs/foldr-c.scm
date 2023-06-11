(define (add x y) 
       (+ x y))

(define (foldr func end lst)
        (if (null? lst)
            end
            (func (car lst) (foldr func end (cdr lst))))) 

(define (sum lst)
        (foldr add 0 lst))

(sum (quote (1 2 3 4)))
