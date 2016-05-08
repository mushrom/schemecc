(let ((display
        (lambda (x)
          (foreign-call "print_scheme_obj" x)))

      (newline
        (lambda (quote ())
          (foreign-call "s_write_char" #\newline)))

      (thing
        (lambda (x y)
          (foreign-call "s_write_char" x)
          (foreign-call "s_write_char" y))))

  (display (cons 1 (cons 2 (cons 3 (quote ())))))
  (newline)
  (thing #\A #\newline))
