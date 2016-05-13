(import (scheme base))
(import (scheme write))

(define (recurse x limit)
  (if (< x limit)
    (begin
      (print x)
      (recurse (+ x 1) limit))
    0))

(recurse 0 10)

(define foo 10)
(set! foo #\Z)
(print foo)
