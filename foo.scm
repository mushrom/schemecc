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

(define (shift-left x amount)
  (if (> amount 0)
    (* 2 (shift-left x (- amount 1)))
    x))

(define call-regs
  '(rdi rsi rdx rcx r8 r9))

(define (immediate? x)
  (or (integer? x)
      (null? x)))

(define (primcall? x)
  (and (list? x)
       (not (null? x))
       (member? (car x) '(add1 + -))))

(define (let? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'let)))

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
       (emit "sub rax, [rsp - " sindex "]")))))

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

(define bindings cadr)
(define body     caddr)

(define (emit-expr x sindex env)
  (cond
    ((immediate? x)
     (emit-comment "immediate int " x)
     (emit "mov rax, " (immediate-rep x)))

    ((let? x)
     (emit-comment "got here")
     (emit-let (bindings x) (body x) sindex env))

    ((variable? x)
     (emit-comment "variable " x)
     (emit "mov rax, [rsp - " (lookup x env) "]"))

    ((primcall? x)
     (emit-primitive-call x sindex env))

    (true (print "wut"))))

(define (compile-program x)
  (emit-flag "bits 64")
  (emit-flag "global scheme_thing")
  (emit-flag "scheme_thing:")
  (emit-expr x 8 '()) ; 8 so we don't overwrite the return pointer
  (emit "ret"))

;(compile-program '(+ 2 (- (+ 40 40) 40)))
(compile-program
  '(let ((x 20)
         (y 60))
     (- (+ x y) 38)))
