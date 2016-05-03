(define (display x)
  (foreign-call print_scheme_obj x))

(define (newline)
  (foreign-call s_write_char #\newline))

(define (print x)
  (display x)
  (newline))

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
