#!/usr/bin/env gojira

(define :mut pretty-last-was-list? #f)
(define (pretty-set-current x)
  (set! last-was-list? (list? x)))

(define (pretty-iter xs indent is-first?)
  (cond
    ((list? xs)
        (if (not is-first?)
          (begin 
             (display #\newline)
             (for-each (lambda (unused) (display #\space)) (iota indent)))
          '())

        (let ((next-indent
               (+ indent
                  (if (or (null? xs) (list? (car xs)))
                      1
                   else
                      2))))

          (display #\()
          (when (not (null? xs))
            (pretty-iter (car xs) next-indent #t)
            (for-var thing in (cdr xs)
                     (pretty-iter thing next-indent #f)))
          (display #\))))

    (true
      (if (not is-first?)
        (if pretty-last-was-list?
          (begin
            (display #\newline)
            (for-each (lambda (unused) (display #\space)) (iota indent)))
          (display #\space))
       else
        '())

      (display xs)))
  
  (pretty-set-current xs))

(define (pretty xs)
  (pretty-iter xs 0 #t)
  (display #\newline))
