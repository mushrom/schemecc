(define (emit :rest args)
  (display #\tab)
  (for-each display args)
  (newline))

(define (emit-comment :rest args)
  (display #\tab)
  (display "; ")
  (for-each display args)
  (newline))

(define (emit-flag :rest args)
  (for-each display args)
  (newline))

(define (emit-label labelspec)
  (for-each display labelspec)
  (display ":")
  (newline))

(define *cur-label* 0)
(define (unique-label)
  (set! *cur-label* (+ *cur-label* 1))
  (list ".label" *cur-label*))

(define (shift-left x amount)
  (if (> amount 0)
    (* 2 (shift-left x (- amount 1)))
    x))

(define call-regs
  '(rdi rsi rdx rcx r8 r9))

(define (immediate? x)
  (or (integer? x)
      (null? x)
      (boolean? x)))

(define (primcall? x)
  (and (list? x)
       (not (null? x))
       (member? (car x) '(add1 + - < > = cons car cdr))))

(define (let? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'let)))

(define (if? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'if)))

(define variable? symbol?)

(define (primcall-op x)
  (car x))

(define (primcall-op-1 x)
  (cadr x))

(define (primcall-op-2 x)
  (caddr x))

(define (immediate-rep x)
  (cond ((integer? x) (shift-left x 2))

        ((null? x)
         47)

        ((boolean? x)
         (if x
           #x9f
           #x1f))

        (true "error: no immediate representation eh")))

(define (emit-primitive-call x sindex env)
  (let ((op (primcall-op x)))
    (emit-comment "primcall " op ": " x)

    (cond
      ((eq? op 'add1)
       (emit-expr (primcall-op-1 x) sindex env)
       (emit "add rax, " (immediate-rep 1)))

      ((eq? op '+)
       (emit-expr (primcall-op-2 x) sindex env)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8) env)
       (emit "add rax, [rsp - " sindex "]"))

      ((eq? op '-)
       (emit-expr (primcall-op-2 x) sindex env)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8) env)
       (emit "sub rax, [rsp - " sindex "]"))

      ((eq? op '<)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr (primcall-op-2 x) sindex env)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8) env)

       (emit "cmp rax, [rsp - " sindex "]")
       (emit-jl L0)

       (emit "mov eax, " (immediate-rep #f))
       (emit-jmp L1)

       (emit-label L0)
       (emit "mov eax, " (immediate-rep #t))

       (emit-label L1)))

      ((eq? op '>)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr (primcall-op-2 x) sindex env)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8) env)

       (emit "cmp rax, [rsp - " sindex "]")
       (emit-jg L0)

       (emit "mov eax, " (immediate-rep #f))
       (emit-jmp L1)

       (emit-label L0)
       (emit "mov eax, " (immediate-rep #t))

       (emit-label L1)))

      ((eq? op '=)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr (primcall-op-2 x) sindex env)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8) env)

       (emit "cmp rax, [rsp - " sindex "]")
       (emit-je L0)

       (emit "mov eax, " (immediate-rep #f))
       (emit-jmp L1)

       (emit-label L0)
       (emit "mov eax, " (immediate-rep #t))

       (emit-label L1)))

      ((eq? op 'cons)
       (emit-expr (primcall-op-2 x) sindex env)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8) env)

       (emit "mov [esi], rax")
       (emit "mov rax, [rsp - " sindex "]" )
       (emit "mov [esi+8], rax")
       (emit "mov rax, rsi")
       (emit "or rax, 1")
       (emit "add rsi, 16"))

      ((eq? op 'car)
       (emit-expr (primcall-op-1 x) sindex env)
       (emit "and rax, ~7")
       (emit "mov rax, [rax]"))

      ((eq? op 'cdr)
       (emit-expr (primcall-op-1 x) sindex env)
       (emit "and rax, ~7")
       (emit "add rax, 8")
       (emit "mov rax, [rax]"))

      (true 'asdf))))

(define (extend-env var sindex env)
  (cons (list var sindex) env))

(define (lookup x env)
  (emit-comment "trying " x " at " env)
  (cond ((null? env)
         'not-found-sorry)

        ((eq? (caar env) x)
         (cadar env))

        (true
          (lookup x (cdr env)))))

(define bindings cadr)
(define body     caddr)

(define (emit-let bindings body sindex env)
  (define (f b* new-env sindex)
    (emit-comment b* " " sindex " " new-env)
    (cond
      ((null? b*)
       (emit-comment "got here, " body)
       (emit-expr body sindex new-env))

      (true
        (let ((b (car b*)))
          (emit-expr (cadr b) sindex env)
          (emit "mov [rsp - " sindex "], eax")
          (f (cdr b*)
             (extend-env (car b) sindex new-env)
             (+ sindex 8))))))
  
  (f bindings env sindex))

(define (emit-cmp comp reg)
  (emit "cmp " reg ", " comp))

(define (emit-je labelspec)
  (apply emit (cons "je " labelspec)))

(define (emit-jl labelspec)
  (apply emit (cons "jl " labelspec)))

(define (emit-jg labelspec)
  (apply emit (cons "jg " labelspec)))

(define (emit-jmp labelspec)
  (apply emit (cons "jmp " labelspec)))

(define (emit-if test conseq altern sindex env)
  (let ((L0 (unique-label))
        (L1 (unique-label)))
    (emit-expr test sindex env)
    (emit-cmp (immediate-rep #f) 'rax)
    (emit-je L0)

    (emit-expr conseq sindex env)
    (emit-jmp L1)

    (emit-label L0)
    (emit-expr altern sindex env)

    (emit-label L1)))

(define (emit-expr x sindex env)
  (cond
    ((immediate? x)
     (emit-comment "immediate " x)
     (emit "mov rax, " (immediate-rep x)))

    ((let? x)
     (emit-comment "got here")
     (emit-let (bindings x) (body x) sindex env))

    ((if? x)
     (emit-comment "if " (cadr x))
     (emit-if (cadr x) (caddr x) (caddr (cdr x)) sindex env))

    ((variable? x)
     (emit-comment "variable " x)
     (emit "mov rax, [rsp - " (lookup x env) "]"))

    ((primcall? x)
     (emit-primitive-call x sindex env))

    (true (print "wut"))))

(define (compile-program x)
  (emit-flag "bits 64")
  (emit-flag "extern scheme_heap")
  (emit-flag "global scheme_thing")
  (emit-flag "scheme_thing:")
  (emit "mov rsi, scheme_heap")
  (emit-expr x 8 '()) ; 8 so we don't overwrite the return pointer
  (emit "ret"))

;(compile-program '(+ 2 (- (+ 40 40) 40)))
;(compile-program
;  '(let ((x 20)
;         (y 60))
;     (let ((x (+ x y))
;           (y 20))
;       (- (+ x y) 38))))

;(compile-program
;  '(let ((x 21)
;         (y 20))
;     (if (> x y)
;       (cons 1 2)
;       2)))

(compile-program
  '(let ((foo (cons 1 (cons 2 ())))
         (bar (cons 3 (cons 4 ()))))
     (cdr (cons foo bar))
     ))
  ;'(cdr (cons (cons 1 (cons 2 ())) (cons 3 (cons 4 ())))))
