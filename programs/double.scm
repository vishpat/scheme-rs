(define (double-each:l s:l)
  (if (null? s) 
      (quote ())
      (cons (* 2 (car s)) (double-each (cdr s)))))

(car (double-each (quote (1 2 3 4))))