(quote (1 2 3))
(define pi 3.14)

(define (area-of-circle r)
          (* pi (* r r)))

(define (area-of-square x)
          (* x x))

(define r (area-of-square 2))
(area-of-circle r)
