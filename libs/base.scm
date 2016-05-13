(define-library (scheme base)
   (export cons car cdr = eq? < > + -
           map for-each)

   (begin
     (define (cons a b)
       (primitive-call cons a b))

     (define (car x)
       (primitive-call car x))

     (define (cdr x)
       (primitive-call cdr x))

     (define (= a b)
       (primitive-call = a b))

     (define (eq? a b)
       (primitive-call = a b))

     (define (< a b)
       (primitive-call < a b))

     (define (> a b)
       (primitive-call > a b))

     (define (+ a b)
       (primitive-call + a b))

     (define (- a b)
       (primitive-call - a b))

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
