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

(define wordsize 8)
(define arg-stack-pos 2)

(define (pointer-index n)
  (* n wordsize))

(define (immediate? x)
  (or (integer? x)
      (null? x)
      (boolean? x)))

(define (primcall? x)
  (and (list? x)
       (not (null? x))
       (member? (car x) '(add1 + - < > = cons car cdr
                          stack-ref closure-ref))))

(define (let? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'let)))

(define (if? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'if)))

(define (labels? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'labels)))

(define (closure? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'closure)))

(define variable? symbol?)

(define (primcall-op x)
  (car x))

(define (primcall-op-1 x)
  (cadr x))

(define (primcall-op-2 x)
  (caddr x))

(define (closure-free closure)
  (if (null? closure)
    '()
    (caddr closure)))

(define (closure-vars closure)
  (if (null? closure)
    '()
    (cadr closure)))

(define (immediate-rep x)
  (cond ((integer? x) (shift-left x 2))

        ((null? x)
         47)

        ((boolean? x)
         (if x
           #x9f
           #x1f))

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
       (emit "sub rax, [rsp - " sindex "]"))

      ((eq? op '<)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr (primcall-op-2 x) sindex)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8))

       (emit "cmp rax, [rsp - " sindex "]")
       (emit-jl L0)

       (emit "mov rax, " (immediate-rep #f))
       (emit-jmp L1)

       (emit-label L0)
       (emit "mov rax, " (immediate-rep #t))

       (emit-label L1)))

      ((eq? op '>)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr (primcall-op-2 x) sindex)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8))

       (emit "cmp rax, [rsp - " sindex "]")
       (emit-jg L0)

       (emit "mov rax, " (immediate-rep #f))
       (emit-jmp L1)

       (emit-label L0)
       (emit "mov rax, " (immediate-rep #t))

       (emit-label L1)))

      ((eq? op '=)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr (primcall-op-2 x) sindex)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8))

       (emit "cmp rax, [rsp - " sindex "]")
       (emit-je L0)

       (emit "mov rax, " (immediate-rep #f))
       (emit-jmp L1)

       (emit-label L0)
       (emit "mov rax, " (immediate-rep #t))

       (emit-label L1)))

      ((eq? op 'cons)
       (emit-expr (primcall-op-2 x) sindex)
       (emit "mov [rsp - " sindex "], rax")
       (emit-expr (primcall-op-1 x) (+ sindex 8))

       (emit "mov [rsi], rax")
       (emit "mov rax, [rsp - " sindex "]" )
       (emit "mov [rsi+8], rax")
       (emit "mov rax, rsi")
       (emit "or rax, 1")
       (emit "add rsi, 16"))

      ((eq? op 'car)
       (emit-expr (primcall-op-1 x) sindex)
       (emit "and rax, ~7")
       (emit "mov rax, [rax]"))

      ((eq? op 'cdr)
       (emit-expr (primcall-op-1 x) sindex)
       (emit "and rax, ~7")
       (emit "add rax, 8")
       (emit "mov rax, [rax]"))

      ((eq? op 'closure-ref)
       (emit "mov rdx, [rsp - 8]")
       (emit "and rdx, ~0b110")
       (emit "mov rax, [rdx + " (pointer-index (+ (primcall-op-1 x) 1)) "]"))

      ((eq? op 'stack-ref)
       (emit "mov rax, [rsp - " (pointer-index (+ arg-stack-pos (primcall-op-1 x))) "]"))

      (true 'asdf))))

(define bindings cadr)
(define body     caddr)

(define (emit-let bindings body sindex)
  (define (f b* sindex)
    (emit-comment b* " " sindex)
    (cond
      ((null? b*)
       (emit-expr body sindex))

      (true
        (let ((b (car b*)))
          (emit-expr (cadr b) sindex)
          (emit "mov [rsp - " sindex "], rax")
          (f (cdr b*)
             (+ sindex 8))))))
  
  (f bindings sindex))

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

(define (emit-if test conseq altern sindex)
  (let ((L0 (unique-label))
        (L1 (unique-label)))
    (emit-expr test sindex)
    (emit-cmp (immediate-rep #f) 'rax)
    (emit-je L0)

    (emit-expr conseq sindex)
    (emit-jmp L1)

    (emit-label L0)
    (emit-expr altern sindex)

    (emit-label L1)))

(define (emit-closure x sindex)
  (define (iter x cindex)
    (when (not (null? x))
      (emit-primitive-call (car x) sindex)
      (emit "mov [rsi + " (pointer-index (+ cindex 1)) "], rax")
      (iter (cdr x) (+ cindex 1))))

  (emit-comment "making closure for " (cadr x)
                ", capturing " (cddr x))

  (emit "mov rax, " (apply values (cadr x)))
  (emit "mov [rsi], rax")
  (iter (cddr x) 0)
  (emit "mov rax, rsi")
  (emit "or rax, 0b110")
  (emit "add rsi, " (+ wordsize (* wordsize (length (cddr x))))))

(define (emit-funcall x sindex)
  (define new-stack-pos 3)
  
  (define (args-iter args i)
    (when (not (null? args))
      (emit-expr (car args) (+ sindex (pointer-index i)))
      (emit "mov [rsp - " (+ sindex (pointer-index i)) "], rax")
      (args-iter (cdr args) (+ i 1))))

  (emit-comment "operator: " (car x))
  (emit-expr (car x) sindex)
  (emit "mov rdi, rax")
  (args-iter (cdr x) new-stack-pos)

  (emit-comment "stack index: " sindex)
  (emit "sub rsp, " sindex)
  (emit "mov [rsp - 16], rdi")
  (emit "and rdi, ~0b110")
  (emit "mov rdi, [rdi]")
  (emit "call rdi")
  (emit "mov rdi, [rsp - 16]")
  (emit "add rsp, " sindex))

(define (emit-expr x sindex)
  (emit-comment "expression: " x)
  (cond
    ((immediate? x)
     (emit-comment "immediate " x)
     (emit "mov rax, " (immediate-rep x)))

    ((let? x)
     (emit-comment "got here")
     (emit-let (bindings x) (body x) sindex))

    ((if? x)
     (emit-comment "if " (cadr x))
     (emit-if (cadr x) (caddr x) (caddr (cdr x)) sindex))

    ((variable? x)
     (emit-comment "Got variable reference.")
     (emit-comment "This shouldn't happen, should have been caught in var. ref pass" x)
     (emit-comment "todo: error out here")
     (emit "rawwwwwwwwwr!"))

    ((primcall? x)
     (emit-primitive-call x sindex))

    ((closure? x)
     (emit-closure x sindex))

    ((list? x)
     (emit-funcall x sindex))

    (true (print "wut"))))

;; code label -> code for the label
(define (label-code-body x)
  (caddr (cdadr x)))

;; code label -> free var. list
(define (label-code-free-vars x)
  (cadr (cadr x)))

(define (emit-label-code x labels)
  (emit-comment (length (label-code-free-vars x)))
  (emit-label (car x))
  (emit-expr (label-code-body x)
             (pointer-index (+ arg-stack-pos
                               (length (label-code-free-vars x)))))
  (emit "ret"))

(define (emit-labels x labels)
  (when (not (null? x))
    (emit-comment "emitting " (car x))
    (emit-label-code (car x) labels)
    (emit-labels (cdr x) labels)))

(define (emit-program x)
  (if (labels? x)
    (begin
      (emit-flag "bits 64")
      (emit-flag "extern scheme_heap")
      (emit-flag "global scheme_thing")
      (emit-flag "scheme_thing:")
      (emit "mov rsi, scheme_heap")

      ; emit main program code
      (emit-flag ".scheme_entry:")
      (emit-expr (caddr x) (pointer-index arg-stack-pos))
      (emit "ret")

      ; emit code for labels
      (emit-flag ".scheme_labels:")
      (emit-labels (cadr x) (cadr x)))

    (emit "somethings wrong, expected a program with labels but got " x)))

(define (lambda? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'lambda)))

(define lambda-args cadr)
(define lambda-body cddr)

(define trans-lambda-args cadr)
(define trans-lambda-body cdddr)
(define trans-lambda-free caddr)

(define (gen-free-vars x defined-vars)
  (cond
    ((null? x)
     '())

    ((let? x)
     (gen-free-vars
       (body x)
       (append (map car (cdr x)) defined-vars)))

    ((primcall? x)
     (gen-free-vars (cdr x) defined-vars))

    ((variable? x)
     (if (not (member? x defined-vars))
       (list x)
       '()))

    ((lambda? x)
     (gen-free-vars
       (lambda-body x)
       (append (lambda-args x) defined-vars)))

    ((list? x)
     (append (gen-free-vars (car x) defined-vars)
             (gen-free-vars (cdr x) defined-vars)))

    (true
      '())))

(define (transform-lambdas x)
  (cond
    ((null? x)
     '())

    ((lambda? x)
     (cons
       'lambda
       (cons
         (lambda-args x)
         (cons
           (gen-free-vars (lambda-body x) (lambda-args x))
           (transform-lambdas (cddr x))))))

    ((list? x)
     (cons
       (transform-lambdas (car x))
       (transform-lambdas (cdr x))))

    (true x)))

(define (gen-labels x)
  (define labels '())

  (define (gen-closure x)
    (let ((label (unique-label)))
      (set! labels
        (cons
          (list label
                (list 'code
                      (trans-lambda-args x)
                      (trans-lambda-free x)
                      (apply values (replace-lambdas (trans-lambda-body x)))))
          labels))

      (list 'closure label (apply values (trans-lambda-free x)))))

  (define (replace-lambdas x)
    (cond
      ((lambda? x)
       (gen-closure x))

      ((null? x)
       '())

      ((list? x)
       (cons
         (replace-lambdas (car x))
         (replace-lambdas (cdr x))))

      (true x)))

  (let ((new-body (replace-lambdas x)))
    (list 'labels labels new-body)))

(define (list-index obj xs)
  (define (list-index-loop obj xs count)
    (cond
      ((null? xs)
        #f)
      ((eq? (car xs) obj)
        count)
      (true
        (list-index-loop obj (cdr xs) (+ count 1)))))

  (list-index-loop obj xs 0))

(define (resolve-var-refs x closure env)
  (cond
    ((null? x) '())

    ((closure? x)
     (cons (car x)
        (cons (cadr x)
          (resolve-var-refs (cddr x) closure env))))

    ((let? x)
     (cons (car x)
        (cons
          (map (lambda (b)
                 (cons (car b)
                       (resolve-var-refs (cdr b) closure env)))
               (cadr x))
          ;(cadr x)
          (resolve-var-refs (cddr x) closure
                            (append env (map car (bindings x)))))))

    ((or (primcall? x)
         (if? x))
     (cons (car x)
           (resolve-var-refs (cdr x) closure env)))

    ((variable? x)
     (cond
       ((member? x env)
        (list 'stack-ref (list-index x env)))

       ((member? x (closure-free closure))
        (list 'closure-ref (list-index x (closure-free closure))))

       (true
         (emit-comment "didn't find " x ". (todo: error out here)")
         'unfound-variable)))

    ((list? x)
     (cons
       (resolve-var-refs (car x) closure env)
       (resolve-var-refs (cdr x) closure env)))

    (true x)))

(define (resolve-code-label-var-refs x)
  (let ((code-def (cadr x)))
    (list (car x)
       (list (car code-def)
             (cadr code-def)
             (caddr code-def)
             (resolve-var-refs (label-code-body x) code-def (cadr code-def))))))

(define (resolve-labels-var-refs x)
  (list
    'labels
    (map resolve-code-label-var-refs (cadr x))
    (resolve-var-refs (caddr x) '() '())))

(load! "pretty.scm")

(define (compile-program x)
  ;(emit-expr x 8 '() '()) ; 8 so we don't overwrite the return pointer
  ;(emit (gen-labels x))
  ;(pretty (gen-labels (transform-lambdas x) '()))
  ;(pretty (transform-lambdas x) '())
  ;(emit-expr (gen-labels (transform-lambdas x) '()) 8 '() '())
  ;(emit-program (gen-labels (transform-lambdas x) '()))
  ;(pretty (resolve-labels-var-refs (gen-labels (transform-lambdas x) '())))
  (emit-program (resolve-labels-var-refs (gen-labels (transform-lambdas x) '())))
  )

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

;(compile-program
;  '(let ((foo (cons 1 (cons 2 ())))
;         (bar (cons 3 (cons 4 ()))))
;
;     (let ((baz (cons foo bar)))
;       (cons baz baz))))
  ;'(cdr (cons (cons 1 (cons 2 ())) (cons 3 (cons 4 ())))))

(compile-program
  '(let ((x 10)
         (double
           (lambda (y) (+ y y)))

         (add
           (lambda (x y) (+ x y)))

         (curry
           (lambda (x)
             (lambda (y)
               (lambda ()
                 (+ x y))
               ))))

     (if (< x 5)
      (double (((curry x) 10)))
      (add 20 22))))

;(compile-program
;  '(let ((x 10)
;         (double
;           (lambda (y) (+ y y))))
;
;     (double 10)))

;(compile-program
;  '(let ((x 5))
;     (((lambda (y)
;         (lambda ()
;           (+ x y))) 10))))

;(compile-program
;  '(let ((double (lambda (x) (lambda () (+ x x)))))
;     ((double 21))))
