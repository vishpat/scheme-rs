(quote (1 2 3))
(define pi 3.14)

(define (area-of-circle r)
          (* pi (* r r)))

(define (area-of-square x)
          (* x x))

(area-of-circle (area-of-square 3))
