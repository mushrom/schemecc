(define-library (scheme base)
   (export (write newline map for-each print))

   (begin
     (define (write x)
       (foreign-call print_scheme_obj x))

     (define (newline)
       (foreign-call s_write_char #\newline))

     (define (print x)
       (write x)
       (newline))

     (define (map f xs)
       (if (= xs (quote ()))
         (quote ())
         (cons (f (car xs))
               (map f (cdr xs)))))

     (define (for-each f xs)
       (if (= xs (quote ()))
         (quote ())
         (begin
           (f (car xs))
           (for-each f (cdr xs)))))))
