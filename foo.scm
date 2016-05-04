#!/usr/bin/env gojira

;; XXX: compatibility with scheme cond and gojira's incomplete cond
(define else #t)

(define (emit :rest args)
  (display #\tab)
  (for-each display args)
  (newline))

(define (emit-comment :rest args)
  (display #\tab)
  (display "; ")
  (for-each write args)
  (newline))

(define (emit-flag :rest args)
  (for-each display args)
  (newline))

(define (emit-label labelspec)
  (for-each display labelspec)
  (display ":")
  (newline))

(define *cur-label* 0)
(define *cur-temp* 0)

(define (unique-label)
  (set! *cur-label* (+ *cur-label* 1))
  (list ".label" *cur-label*))

(define (unique-temp-var)
  (set! *cur-temp* (+ *cur-temp* 1))
  (string->symbol
    (string-append
      "temp"
      (number->string *cur-temp*))))

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
      (boolean? x)
      (char? x)))

(define (primcall? x)
  (and (list? x)
       (not (null? x))
       (member? (car x) '(add1 + - < > = cons car cdr begin
                          vector vector-ref vector-set!
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

(define (foreign-call? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'foreign-call)))

(define (set!? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'set!)))

(define (library-definition? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'define-library)))

(define variable? symbol?)

(define (primcall-op x)
  (car x))

(define (primcall-op-1 x)
  (cadr x))

(define (primcall-op-2 x)
  (caddr x))

(define (primcall-op-3 x)
  (caddr (cdr x)))

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

        ((char? x)
         (+ (shift-left (char->integer x) 8)
            #x0f))

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

       (emit "mov [rbp], rax")
       (emit "mov rax, [rsp - " sindex "]" )
       (emit "mov [rbp+8], rax")
       (emit "mov rax, rbp")
       (emit "or rax, 1")
       (emit "add rbp, 16"))

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

      ((eq? op 'begin)
       (emit-comment (cdr x))
       (emit-expr-list (cdr x) sindex))

      ((eq? op 'vector)
       (emit-comment "making vector")

       (let ((veclen (length (cdr x))))
         (define (loopy args i)
           (when (not (null? args))
             (emit-expr (car args) sindex)
             (emit "mov [rbp + " (pointer-index i) "], rax")
             (loopy (cdr args) (+ i 1))))

         (emit "mov rax, " (immediate-rep veclen))
         (emit "mov [rbp], rax")
         (loopy (cdr x) 1)
         (emit "mov rax, rbp")
         (emit "or rax, 0b010")
         (emit "add rbp, " (pointer-index (+ 1 veclen)))))

       ((eq? op 'vector-ref)
        ; todo: add length checking and error out when appropriate
        (emit-expr (primcall-op-2 x) sindex)
        (emit "mov [rsp - " sindex "], rax")
        (emit-expr (primcall-op-1 x) sindex)

        (emit "mov rdx, [rsp - " sindex "]")
        (emit "shl rdx, 1") ; numbers are already the number multiplied by 4, so
                            ; we can shift left by one to make it multiplied by 8
        (emit "and rax, ~0b010")
        (emit "mov rax, [rax + rdx + 8]"))

       ((eq? op 'vector-set!)
        (emit-expr (primcall-op-2 x) sindex)
        (emit "mov [rsp - " sindex "], rax")

        (emit-expr (primcall-op-3 x) (+ sindex wordsize))
        (emit "mov [rsp - " (+ sindex wordsize) "], rax")

        (emit-expr (primcall-op-1 x) (+ sindex wordsize wordsize))

        (emit "mov rdx, [rsp - " sindex "]")
        (emit "mov [rsp - " sindex "], rax")
        (emit "shl rdx, 1")
        (emit "add rdx, rax")
        (emit "and rdx, ~0b010")
        (emit "mov rax, [rsp - " (+ sindex wordsize) "]")
        (emit "mov [rdx + 8], rax")
        (emit "mov rax, [rsp - " sindex "]"))

      (true 'asdf))))

(define (emit-expr-list xs sindex)
  (when (not (null? xs))
    (emit-comment "emitting next expression in list")
    (emit-expr (car xs) sindex)
    (emit-expr-list (cdr xs) sindex)))

(define bindings cadr)
(define body     cddr)

(define (emit-let bindings body sindex)
  (define (f b* sindex)
    (emit-comment b* " " sindex)
    (cond
      ((null? b*)
       (emit-expr-list body sindex))

      (true
        (let ((b (car b*)))
          (emit-comment "binding " (car b) " => " (cadr b))
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
      (emit "mov [rbp + " (pointer-index (+ cindex 1)) "], rax")
      (iter (cdr x) (+ cindex 1))))

  (emit-comment "making closure for " (cadr x)
                ", capturing " (cddr x))

  (emit "mov rax, " (apply values (cadr x)))
  (emit "mov [rbp], rax")
  (iter (cddr x) 0)
  (emit "mov rax, rbp")
  (emit "or rax, 0b110")
  (emit "add rbp, " (+ wordsize (* wordsize (length (cddr x))))))

(define (emit-funcall x sindex)
  (define new-stack-pos 3)
  
  (define (args-iter args i)
    (when (not (null? args))
      (emit-expr (car args) (+ sindex (pointer-index i)))
      (emit "mov [rsp - " (+ sindex (pointer-index i)) "], rax")
      (args-iter (cdr args) (+ i 1))))

  (args-iter (cdr x) new-stack-pos)

  (emit-comment "operator: " (car x))
  (emit-expr (car x) sindex)
  (emit "mov rbx, rax")

  (emit-comment "stack index: " sindex)
  (emit "sub rsp, " sindex)
  (emit "mov [rsp - 16], rbx")
  (emit "and rbx, ~0b110")
  (emit "mov rbx, [rbx]")
  (emit "call rbx")
  (emit "mov rbx, [rsp - 16]")
  (emit "add rsp, " sindex))

(define (emit-foreign-call x sindex)
  (define (iter args regs)
    (when (and (not (null? args))
               (not (null? regs)))
      (emit-expr (car args) sindex)
      (emit-comment "next arg register: " (cdr regs))
      (emit "mov " (car regs) ", rax")
      (iter (cdr args) (cdr regs))))

  (iter (cddr x) call-regs)
  (emit-flag "extern " (primcall-op-1 x))
  (emit "sub rsp, " sindex)
  (emit "call " (primcall-op-1 x))
  (emit "add rsp, " sindex))

(define (emit-expr x sindex)
  ;(emit-comment "expression: " x)
  ;(emit-comment "============================")
  (cond
    ((immediate? x)
     (emit-comment "immediate " x)
     (emit "mov rax, " (immediate-rep x)))

    ((let? x)
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

    ((foreign-call? x)
     (emit-foreign-call x sindex))

    ((list? x)
     (emit-funcall x sindex))

    (true (print "wut"))))

;; code label -> code for the label
(define (label-code-body x)
  (cddr (cdadr x)))

;; code label -> free var. list
(define (label-code-free-vars x)
  (cadr (cadr x)))

(define (emit-label-code x labels)
  (emit-comment (label-code-body x))
  (emit-label (car x))
  (emit-expr-list (label-code-body x)
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
      (emit "push rbp")
      (emit "push rbx")
      (emit "mov rbp, scheme_heap")

      ; emit main program code
      (emit-flag ".scheme_entry:")
      (emit-expr (caddr x) (pointer-index arg-stack-pos))
      (emit "pop rbx")
      (emit "pop rbp")
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

(define (if-condition x)
  (cadr x))

(define (if-path-1 x)
  (caddr x))

(define (if-path-2 x)
  (caddr (cdr x)))

(define (if-expressions x)
  (cdr x))

(define (p-debug x)
  (print x)
  x)

(define (gen-free-vars x defined-vars)
  (cond
    ((null? x)
     '())

    ((let? x)
     (gen-free-vars
       (body x)
       (append (map car (cadr x)) defined-vars)))

    ((primcall? x)
     (gen-free-vars (cdr x) defined-vars))

    ((if? x)
     (gen-free-vars (if-expressions x) defined-vars))

    ((foreign-call? x)
     (gen-free-vars (cddr x) defined-vars))

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

(define (get-lib-field key lib)
  (print lib)
  (cond
    ((null? lib)
     #f)

    ((eq? key (caar lib))
      (cdr (car lib)))

    (else
      (get-lib-field key (cdr lib)))))

(define (change-lib-field key new-value lib)
  (p-debug (cond
    ((null? lib)
     '())

    ((eq? key (caar lib))
     (cons
       (list key new-value)
       (change-lib-field key new-value (cdr lib))))

    (else
      (cons
        (car lib)
        (change-lib-field key new-value (cdr lib)))))))

(define (construct-library-definition name libspec)
  (cons
    'define-library
    (cons
      name
      libspec)))

(define (library-fields x)
  (cddr x))

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

    ((foreign-call? x)
     (cons 'foreign-call
       (cons (cadr x)
         (resolve-var-refs (cddr x) closure env))))

    ((variable? x)
     (cond
       ((member? x env)
        (list 'stack-ref (list-index x env)))

       ((member? x (closure-free closure))
        (list 'closure-ref (list-index x (closure-free closure))))

       (true
         (emit-comment "didn't find " x ". (todo: error out here)")
         'unfound-variable)))

    ((library-definition? x)
     (print (library-fields x))
     (construct-library-definition
       (cadr x)
       (change-lib-field 'begin
         (resolve-var-refs (get-lib-field 'begin (library-fields x)) '() '())
         (library-fields x))))

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
             (apply values
                    (resolve-var-refs (label-code-body x)
                                      code-def
                                      (cadr code-def)))))))

(define (resolve-labels-var-refs x)
  (list
    'labels
    (map resolve-code-label-var-refs (cadr x))
    (resolve-var-refs (caddr x) '() '())))

(define (list-remove obj xs)
  (cond
    ((null? xs) '())

    ((eq? obj (car xs))
     (list-remove obj (cdr xs)))

    (true
      (if (list? (car xs))
        (cons (list-remove obj (car xs))
              (list-remove obj (cdr xs)))
       else
        (cons (car xs)
              (list-remove obj (cdr xs)))))))

(define (list-remove-objs objxs xs)
  (cond
    ((or (null? objxs)
         (null? xs))
     xs)

    (true
      (list-remove (car objxs)
        (list-remove-objs (cdr objxs) xs)))))

(define (list-replace* old new xs)
  (cond
    ((null? xs) '())

    ((eq? old (car xs))
     (cons new (list-replace* old new (cdr xs))))

    (true
      (if (list? (car xs))
        (cons (list-replace* old new (car xs))
              (list-replace* old new (cdr xs)))
       else
        (cons (car xs)
              (list-replace* old new (cdr xs)))))))

(define (list-replace-objs* objxs xs)
  (cond
    ((or (null? objxs)
         (null? xs))
     xs)

    (true
      (list-replace* (caar objxs) (cadar objxs)
        (list-replace-objs* (cdr objxs) xs)))))

(define (remove-repeat xs)
  (if (null? xs)
    '()
    (let ((next (remove-repeat (cdr xs))))
      (if (member? (car xs) next)
        next
        (cons (car xs) next)))))

(define (intersect xs ys)
  (filter (lambda (x)
            (and (member? x xs)
                 (member? x ys)))
          xs))

(define (expand fun iter data)
  (cond
    ((null? iter)
     data)

    (true
      (fun (car iter)
           (expand fun (cdr iter) data)))))

(define (construct-lambda args body)
  (cons 'lambda
    (cons args body)))

(define (construct-let binds body)
  (cons 'let
    (cons binds body)))

(define (construct-vector :rest args)
  (cons 'vector args))

(define (construct-let-binding name value)
  (list name value))

(define (construct-vector-set vec index value)
  (cons 'vector-set!
    (cons vec
      (cons index
        (cons value '())))))

(define (construct-vector-ref vec index)
  (cons 'vector-ref
    (cons vec
      (cons index '()))))

(define (construct-if condition path1 path2)
  (list 'if condition path1 path2))

(define (construct-begin :rest code)
  (cons 'begin code))

(define (change-let-binding name new-value binds)
  (when (not (null? binds))
    (emit-comment name " " (car binds)))

  (cond
    ((null? binds)
     '())

    ((eq? name (caar binds))
     (emit-comment "got here")
     (cons
       (construct-let-binding name new-value)
       (change-let-binding name new-value (cdr binds))))

    (true
      (cons
        (car binds)
        (change-let-binding name new-value (cdr binds))))))

(define (get-binding name binds)
  (assq name binds))

(define (vectorise-mutable-bindings var binds)
  (let ((bind-data (get-binding var binds)))
    (change-let-binding var (construct-vector bind-data) binds)))

(define (replace-assignments x vars)
  (cond
    ((null? x)
     '())

    ((and (variable? x)
          (member? x vars))
     (construct-vector-ref x 0))

    ((set!? x)
     (construct-vector-set
       (primcall-op-1 x)
       0
       (replace-assignments (primcall-op-2 x) vars)))

    ((list? x)
      (cons
        (replace-assignments (car x) vars)
        (replace-assignments (cdr x) vars)))

    (true x)))

(define (insert-lets vars xs)
  (emit-comment "replacing " vars)
  (list (construct-let
      (list (list (car vars)
                  (list 'vector (cadr vars))))
      xs)))

(define (transform-assignments x)
  (cond
    ((null? x)
     (list '() '()))

    ((lambda? x)
     (let* ((assigns    (transform-assignments (lambda-body x)))
            (found-vars (intersect (lambda-args x) (car assigns)))
            (var-pairs  (map (lambda (x)
                               (list x (unique-temp-var)))
                             found-vars)))

       (for arg in (lambda-args x)
          (when (member? arg (car assigns))
            (emit-comment "lambda has assignment statement for argument " arg " in it's body")))

       ;assigns
       (list
         (list-remove-objs (lambda-args x) (car assigns))
         (construct-lambda
           (list-replace-objs* var-pairs (lambda-args x))
           (expand insert-lets
                   var-pairs
                   (replace-assignments (lambda-body x) (car assigns)))))))

    ((let? x)
     (let* ((left  (transform-assignments (bindings x)))
            (right (transform-assignments (body x)))
            (assigns (append (car left) (car right)))
            (found-vars (intersect (map car (bindings x)) assigns)))

       (for (key value) in (bindings x)
          (when (member? key assigns)
            (emit-comment "let has assignment statement for binding " key " in it's body")))

       (list
         (list-remove-objs (map car (bindings x)) assigns)
         (construct-let
           (expand vectorise-mutable-bindings
                   found-vars
                   (cadr left))
           ;(cadr left)
           (replace-assignments (cadr right) found-vars)))))

    ((set!? x)
     (let ((set-body (transform-assignments (primcall-op-2 x))))
       (list (list (primcall-op-1 x))
             (cons 'set!
                   (cons
                     (primcall-op-1 x)
                     (cdr set-body))))))
    ((list? x)
     (let ((left  (transform-assignments (car x)))
           (right (transform-assignments (cdr x))))

       (list
         (append
           (car left)
           (car right))

         (cons
           (cadr left)
           (cadr right)))))

    (true (list '() x))))

(define (define? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'define)))

(define (cond? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'cond)))

(define (expand-define def-spec body)
  (let ((def-id    (cadr  def-spec))
        (def-value (cddr def-spec)))

    (cond
       ((list? def-id)
        (list
          (construct-let
            (list (construct-let-binding
                    (car def-id)
                    (construct-let
                      (list (construct-let-binding (car def-id) #f))
                      (list (list 'set! (car def-id)
                                      (construct-lambda
                                        (cdr def-id)
                                        def-value))
                            (car def-id)))))
            body)))

       ((variable? def-id)
        (list
          (construct-let
            (list (construct-let-binding
                    def-id
                    (construct-let
                      (list (construct-let-binding def-id #f))
                      (list (list 'set! def-id (car def-value))
                            def-id))))
            body)))

       (true
         (error-print "invalid define specification")))))

(define (expand-cond x)
  (define (expand-cond-iter expr)
    (cond
       ((null? expr)
        #f)

       ((eq? (caar expr) 'else)
        (apply construct-begin (cdar expr)))

       (true
         (construct-if (caar expr)
            (apply construct-begin (cdar expr))
            (expand-cond-iter (cdr expr))))))

  (expand-cond-iter (cdr x)))

(define (rewrite-core-syntax x)
  (cond
    ((null? x)
     '())

    ((and (list? x)
          (define? (car x)))
     (rewrite-core-syntax
       (expand-define (car x) (cdr x))))

    ((cond? x)
     (rewrite-core-syntax (expand-cond x)))

    ((list? x)
      (cons
        (rewrite-core-syntax (car x))
        (rewrite-core-syntax (cdr x))))

    (true x)))

(load! "pretty.scm")

(define error-port (open "/dev/stderr" "w"))

(define (error-print :rest x)
  (display "ERROR: " error-port)
  (for thing in x
    (display thing error-port))
  (display #\newline error-port))

(define (compile-program x)
  (call/cc
    (lambda (escape)
      (let* ((expanded       (rewrite-core-syntax x))
             (assigned-vars  (transform-assignments expanded)))
        (if (null? (car assigned-vars))
          (begin
            (emit-program (resolve-labels-var-refs (gen-labels (transform-lambdas (cadr assigned-vars)) '())))
            ;(pretty (resolve-labels-var-refs (gen-labels (transform-lambdas (cadr assigned-vars)) '())))
            ;(pretty assigned-vars)
            ;(pretty assigned-vars)
            ;(pretty expanded)
            'success)
         else
          (begin
            (for remaining in (car assigned-vars)
                 (error-print "set!: \"" remaining "\" is not defined, or not in scope of set!"))
            (escape 'error)))))))

;; todo: remove this once proper ports are implemented in gojira
(define (eof-object? x)
  (eq? x #f))

(define (read-program port)
  (let ((buf (read port)))
    (if (not (eof-object? buf))
      (cons buf (read-program port))
      '())))

(define (parse-args args)
  (cond
    ((null? args) '())

    ((eq? (string-ref (car args) 0) #\-)
     (cons
       (map list->string
               (list-split (string->list (car args)) #\:))
       (parse-args (cdr args))))

    (else
      (cons (car args)
            (parse-args (cdr args))))))

(define (argument-flags parsed)
  (filter list? parsed))

(define (argument-fields parsed)
  (filter string? parsed))

(define (arguments-contain-flag? flag args)
  (member? flag (map car (argument-flags args))))

; program entry
(let ((args (parse-args *arguments*)))
  (if (>= (length (argument-fields args)) 1)
    ; TODO: handle actually outputing to different files
    (for filename in (argument-fields args)
      (emit-comment "compiling " filename)
      (let* ((port (open filename "r"))
             (program (cons 'begin (read-program port))))

        ;(pretty program)
        (emit-comment "compilation result: " (compile-program program))))

  else
    (error-print "Need filename to compile")))
