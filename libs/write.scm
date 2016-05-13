(define-library (scheme write)
   (import (scheme base))
   (export write newline print)

   (begin
     (define (write x)
       (foreign-call print_scheme_obj x))

     (define (newline)
       (foreign-call s_write_char #\newline))

     (define (print x)
       (write x)
       (newline))))
