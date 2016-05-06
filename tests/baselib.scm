(import (scheme base))

(print 1234)

(for-each write
    (cons #\a
      (cons #\b
        (cons #\c
              ()))))

(newline)
