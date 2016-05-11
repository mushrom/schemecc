(import (scheme base))

(define (foo x)
  (define (bar)
    (+ x 1))

  (bar))

(print (foo 123))
0
