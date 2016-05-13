(let ((v (primitive-call vector #\a #\b #\c)))
  (primitive-call vector-set! v 1 10)
  (primitive-call cons
    (primitive-call vector-ref v 2)
    (primitive-call cons
      (primitive-call vector-ref v 1)
      (primitive-call cons
        (primitive-call vector-ref v 0)
        (quote ())))))
