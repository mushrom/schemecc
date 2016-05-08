(let ((foo (cons 1 (cons 2 (quote ()))))
      (bar (cons 3 (cons 4 (quote ())))))

  (let ((baz (cons foo bar)))
    (cons baz baz)))
