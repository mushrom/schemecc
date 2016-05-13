(define-library (scheme base)
   (export map for-each)

   (begin
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
