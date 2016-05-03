(let ((blarg
        (lambda (c)
            (set! c 10)))
      
      (recurse
        (let ((recurse #f))
          (set! recurse
            (lambda (x)
              (if (> x 0)
                (begin
                  (foreign-call print_scheme_obj x)
                  (foreign-call s_write_char #\newline)
                  (set! x (- x 1))
                  (recurse x))
                0)))
          recurse)))

  (blarg (lambda () #\a))
  (recurse 10))
