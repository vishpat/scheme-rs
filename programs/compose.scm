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
