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

(define (emit-primitive-call x sindex)
  (let ((op (primcall-op x)))
    (emit-comment "primcall " op ": " x)
    (cond
      ((eq? op 'add1)
       (emit-expr (primcall-op-1 x) sindex)
       (emit "add rax, " (immediate-rep 1)))

      ((eq? op '+)
       (emit-expr (primcall-op-2 x) sindex)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8))
       (emit "add rax, [rsp - " sindex "]"))

      ((eq? op '-)
       (emit-expr (primcall-op-2 x) sindex)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8))
       (emit "sub rax, [rsp - " sindex "]")))))

(define (emit-expr x sindex)
  (cond
    ((immediate? x)
     (emit-comment "immediate int " x)
     (emit "mov rax, " (immediate-rep x)))

    ((primcall? x)
     (emit-primitive-call x sindex))

    (true (print "wut"))))

(define (compile-program x)
  (emit-flag "bits 64")
  (emit-flag "global scheme_thing")
  (emit-flag "scheme_thing:")
  (emit-expr x 8) ; 8 so we don't overwrite the return pointer
  (emit "ret"))

(compile-program
  '(+ 2 (- (+ 40 40) 40)))
