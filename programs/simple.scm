(define x 20)
(define y 20)
(define z 
  (if (< x y) 
    (+ 1 2) 
    (+ 4 4)))
(+ z (+ x y))
