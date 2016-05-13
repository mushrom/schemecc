(import (scheme base))
(import (scheme write))

(print 1234)

(for-each write
    (cons #\a
      (cons #\b
        (cons #\c
              (quote ())))))

(newline)
