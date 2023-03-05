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
