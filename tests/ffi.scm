(import (scheme base))
(import (scheme write))

(let ((thing
        (lambda (x y)
          (foreign-call "s_write_char" x)
          (foreign-call "s_write_char" y))))

  (write (cons 1 (cons 2 (cons 3 (quote ())))))
  (newline)
  (thing #\A #\newline))
