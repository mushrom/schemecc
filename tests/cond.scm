(import (scheme base))
(import (scheme write))

(define (meh x)
  (cond
    ((= x 1) #\a)
    ((= x 2) #\b)
    ((= x 3) #\c)
    (else    #\d)))

(for-each print
  (map meh
    (cons 1
      (cons 2
        (cons 3
          (cons 123 (quote ())))))))
