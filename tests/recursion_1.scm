(import (scheme base))

(let ((print-thing
        (lambda (x)
          (foreign-call print_scheme_obj x)
          (foreign-call s_write_char #\newline))))

  (let ((foo
          (lambda (f x)
            (if (> x -5)
              (begin
                (print-thing x)
                (f f (- x 1)))
              x))))
    (foo foo 5)))
