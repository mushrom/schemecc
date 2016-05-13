(import (scheme base))

(let ((x (+ 5 5))
      (double
        (lambda (y) (+ y y)))

      (add
        (lambda (x y)
          (begin
            (+ (+ x x) y)
            (+ (+ x x) y)
            (+ (+ x x) y))
          (+ x y)))

      (curry
        (lambda (x)
          (lambda (y)
            (lambda ()
              (+ x y))))))

  (+ x 1)

  (if (> x 5)
    (double (((curry x) 10)))
    (add 20 22)))
