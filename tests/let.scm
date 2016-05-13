(import (scheme base))

(let ((x 20)
      (y 60))
  (let ((x (+ x y))
        (y 20))
    (- (+ x y) 38)))
