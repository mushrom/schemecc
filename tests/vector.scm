(let ((v (vector #\a #\b #\c)))
  (vector-set! v 1 10)
  (cons
    (vector-ref v 2)
    (cons
      (vector-ref v 1)
      (cons
        (vector-ref v 0)
        ()))))
