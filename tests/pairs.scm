(let ((foo (cons 1 (cons 2 ())))
      (bar (cons 3 (cons 4 ()))))

  (let ((baz (cons foo bar)))
    (cons baz baz)))
