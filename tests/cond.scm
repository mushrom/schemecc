(define (display x)
  (foreign-call print_scheme_obj x))

(define (newline)
  (foreign-call s_write_char #\newline))

(define (print x)
  (display x)
  (newline))

(define (map f xs)
  (if (= xs ())
    ()
    (cons (f (car xs))
      (map f (cdr xs)))))

(define (for-each f xs)
  (if (= xs ())
    ()
    (begin
      (f (car xs))
      (for-each f (cdr xs)))))

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
          (cons 123 ()))))))
