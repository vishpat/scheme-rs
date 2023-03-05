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
