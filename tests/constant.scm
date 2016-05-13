(import (scheme base))
(import (scheme write))

(define (double x)
  (+ x x))

(let ((a (quote (1 2 3)))
      (b (quote (10 "testing")))
      (c (quote "foo"))
      (d (quote ())))

  (for-each print
    (map double a))
  
  (print b)
  (print c)

  (for-each print d))
