#!/usr/bin/env gojira

;; XXX: compatibility with scheme cond and gojira's incomplete cond
(define else #t)

;; some basic error handling
(define error-port (open "/dev/stderr" "w"))

(define (error-print :rest x)
  (display "ERROR: " error-port)
  (for thing in x
    (display thing error-port))
  (display #\newline error-port))

(define (abandon-hope message)
  (error-print message)
  (exit 1))

(define-syntax assert
  (syntax-rules ()
    ((_ condition value message)
     (if (not (condition value))
       (abandon-hope (list message ":" value))
       value))

    ((_ condition value)
     (if (not (condition value))
       (abandon-hope (list "failed assertion" 'condition "in expression:" value))
       value))))

(define-syntax assert-no-eval
  (syntax-rules ()
    ((_ condition value message)
     (if (not (condition value))
       (abandon-hope '(message ":" value))
       value))

    ((_ condition value)
     (if (not (condition value))
       (abandon-hope '("failed assertion" condition "in expression:" value))
       value))))

(define (emit port :rest args)
  (display #\tab port)
  (for-each (lambda (x)
              (display x port))
            args)
  (newline port))

(define (emit-comment port :rest args)
  (display #\tab port)
  (display "; " port)
  (for-each (lambda (x)
              (write x port))
            args)
  (newline port))

(define (emit-flag port :rest args)
  (for-each (lambda (x)
              (display x port))
            args)
  (newline port))

(define (emit-label port labelspec)
  (cond
    ((list? labelspec)
     (for-each (lambda (x)
                 (display x port))
               labelspec))

    (else
      (display labelspec port)))

  (display ":" port)
  (newline port))

(define *cur-label* 0)
(define *cur-temp* 0)

(define (unique-label)
  (set! *cur-label* (+ *cur-label* 1))
  (string->symbol
    (string-append
      ".label"
      (number->string *cur-label*))))

(define (unique-temp-var)
  (set! *cur-temp* (+ *cur-temp* 1))
  (string->symbol
    (string-append
      ".temp"
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

(define (make-basic-type-check sym)
  (lambda (x)
    (and (list? x)
         (not (null? x))
         (eq? (car x) sym))))

(define (immediate? x)
  (or (integer? x)
      (null? x)
      (boolean? x)
      (char? x)))

;(define (primcall? x)
;  (and (list? x)
;       (not (null? x))
;       (member? (car x) '(add1 + - < > = cons car cdr begin
;                          vector vector-ref vector-set!
;                          assignment-set! assignment-ref
;                          stack-ref closure-ref
;                          string
;                          ))))

(define primcall?        (make-basic-type-check 'primitive-call))
(define variable?        symbol?)
(define let?             (make-basic-type-check 'let))
(define if?              (make-basic-type-check 'if))
(define begin?           (make-basic-type-check 'begin))
(define labels?          (make-basic-type-check 'labels))
(define closure?         (make-basic-type-check 'closure))
(define foreign-call?    (make-basic-type-check 'foreign-call))
(define set!?            (make-basic-type-check 'set!))
(define quoted?          (make-basic-type-check 'quote))
;(define expanded-string? (make-basic-type-check 'string))
(define constant-set!?   (make-basic-type-check 'constant-set!))
(define constant-ref?    (make-basic-type-check 'constant-ref))
(define library-definition?            (make-basic-type-check 'define-library-expanded))
(define unexpanded-library-definition? (make-basic-type-check 'define-library))

(define (expanded-string? x)
  (and (primcall? x)
       (eq? (primcall-op x) 'string)))

(define (assignment-set!? x)
  (and (primcall? x)
       (eq? (primcall-op x) 'assignment-set!)))

(define (assignment-ref? x)
  (and (primcall? x)
       (eq? (primcall-op x) 'assignment-ref)))

(define (primcall-op x)
  (cadr x))

(define (primcall-op-1 x)
  (caddr x))

(define (primcall-op-2 x)
  (caddr (cdr x)))

(define (primcall-op-3 x)
  (caddr (cddr x)))

(define (primcall-body x)
  (cddr x))

(define (closure-free closure)
  (if (null? closure)
    '()
    (caddr closure)))

(define (closure-vars closure)
  (if (null? closure)
    '()
    (cadr closure)))

(define (get-lib-field key lib)
  (cond
    ((null? lib)
     #f)

    ((eq? key (caar lib))
      (cdr (car lib)))

    (else
      (get-lib-field key (cdr lib)))))

(define (change-lib-field key new-value lib)
  (cond
    ((null? lib)
     '())

    ((eq? key (caar lib))
     (cons
       (cons key new-value)
       (change-lib-field key new-value (cdr lib))))

    (else
      (cons
        (car lib)
        (change-lib-field key new-value (cdr lib))))))

(define (construct-library-definition name libspec)
  (cons
    'define-library-expanded
    (cons
      name
      libspec)))

(define (library-fields x)
  (cddr x))

(define (library-name x)
  (cadr x))

(define (find-library-path lib-name suffix)
  (define lib-file (gen-library-file-name lib-name))

  (define (iter paths)
    (cond
      ((null? paths)
       (abandon-hope (list "Could not find library " lib-name)))

      ((exists? (string-concat (list (car paths) "/" lib-file suffix)))
       (string-concat (list (car paths) "/" lib-file suffix)))

      (true
        (iter (cdr paths)))))

  (iter '("." "libs")))

(define (find-library-object-path lib-name)
  (find-library-path lib-name ".o"))

(define (find-library-stub-path lib-name)
  (find-library-path lib-name ".libstub"))

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

(define (emit-primitive-call port x sindex)
  (let ((op (primcall-op x)))
    (emit-comment port "primcall " op ": " x)

    (cond
      ((eq? op 'add1)
       (emit-expr port (primcall-op-1 x) sindex)
       (emit port "add rax, " (immediate-rep 1)))

      ((eq? op '+)
       (emit-expr port (primcall-op-2 x) sindex)
       (emit port "mov [rsp - " sindex "], rax")
       (emit-expr port (primcall-op-1 x) (+ sindex 8))
       (emit port "add rax, [rsp - " sindex "]"))

      ((eq? op '-)
       (emit-expr port (primcall-op-2 x) sindex)
       (emit port "mov [rsp - " sindex "], rax")
       (emit-expr port (primcall-op-1 x) (+ sindex 8))
       (emit port "sub rax, [rsp - " sindex "]"))

      ((eq? op '<)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr port (primcall-op-2 x) sindex)
       (emit port "mov [rsp - " sindex "], rax")
       (emit-expr port (primcall-op-1 x) (+ sindex 8))

       (emit port "cmp rax, [rsp - " sindex "]")
       (emit-jl port L0)

       (emit port "mov rax, " (immediate-rep #f))
       (emit-jmp port L1)

       (emit-label port L0)
       (emit port "mov rax, " (immediate-rep #t))

       (emit-label port L1)))

      ((eq? op '>)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr port (primcall-op-2 x) sindex)
       (emit port "mov [rsp - " sindex "], rax")
       (emit-expr port (primcall-op-1 x) (+ sindex 8))

       (emit port "cmp rax, [rsp - " sindex "]")
       (emit-jg port L0)

       (emit port "mov rax, " (immediate-rep #f))
       (emit-jmp port L1)

       (emit-label port L0)
       (emit port "mov rax, " (immediate-rep #t))

       (emit-label port L1)))

      ((eq? op '=)
       (let ((L0 (unique-label))
             (L1 (unique-label)))
       (emit-expr port (primcall-op-2 x) sindex)
       (emit port "mov [rsp - " sindex "], rax")
       (emit-expr port (primcall-op-1 x) (+ sindex 8))

       (emit port "cmp rax, [rsp - " sindex "]")
       (emit-je port L0)

       (emit port "mov rax, " (immediate-rep #f))
       (emit-jmp port L1)

       (emit-label port L0)
       (emit port "mov rax, " (immediate-rep #t))

       (emit-label port L1)))

      ((eq? op 'cons)
       (emit-expr port (primcall-op-2 x) sindex)
       (emit port "mov [rsp - " sindex "], rax")
       (emit-expr port (primcall-op-1 x) (+ sindex 8))

       (emit port "mov [rbp], rax")
       (emit port "mov rax, [rsp - " sindex "]" )
       (emit port "mov [rbp+8], rax")
       (emit port "mov rax, rbp")
       (emit port "or rax, 1")
       (emit port "add rbp, 16"))

      ((eq? op 'car)
       (emit-expr port (primcall-op-1 x) sindex)
       (emit port "and rax, ~7")
       (emit port "mov rax, [rax]"))

      ((eq? op 'cdr)
       (emit-expr port (primcall-op-1 x) sindex)
       (emit port "and rax, ~7")
       (emit port "add rax, 8")
       (emit port "mov rax, [rax]"))

      ((eq? op 'closure-ref)
       (emit port "mov rdx, [rsp - 8]")
       (emit port "and rdx, ~0b110")
       (emit port "mov rax, [rdx + " (pointer-index (+ (primcall-op-1 x) 1)) "]"))

      ((eq? op 'stack-ref)
       (emit port "mov rax, [rsp - " (pointer-index (+ arg-stack-pos (primcall-op-1 x))) "]"))

      ((eq? op 'vector)
       (emit-comment port "making vector")

       (let ((veclen (length (primcall-body x))))
         (define (loopy args i)
           (when (not (null? args))
             (emit-expr port (car args) sindex)
             (emit port "mov [rbp + " (pointer-index i) "], rax")
             (loopy (cdr args) (+ i 1))))

         (emit port "mov rax, " (immediate-rep veclen))
         (emit port "mov [rbp], rax")
         (loopy (primcall-body x) 1)
         (emit port "mov rax, rbp")
         (emit port "or rax, 0b010")
         (emit port "add rbp, " (pointer-index (+ 1 veclen)))))

       ((or (eq? op 'vector-ref)
            (eq? op 'assignment-ref))
        ; todo: add length checking and error out when appropriate
        (emit-expr port (primcall-op-2 x) sindex)
        (emit port "mov [rsp - " sindex "], rax")
        (emit-expr port (primcall-op-1 x) sindex)

        (emit port "mov rdx, [rsp - " sindex "]")
        (emit port "shl rdx, 1") ; numbers are already the number multiplied by 4, so
        ; we can shift left by one to make it multiplied by 8
        (emit port "and rax, ~0b010")
        (emit port "mov rax, [rax + rdx + 8]"))

       ((or (eq? op 'vector-set!)
            (eq? op 'assignment-set!))
        (emit-expr port (primcall-op-2 x) sindex)
        (emit port "mov [rsp - " sindex "], rax")

        (emit-expr port (primcall-op-3 x) (+ sindex wordsize))
        (emit port "mov [rsp - " (+ sindex wordsize) "], rax")

        (emit-expr port (primcall-op-1 x) (+ sindex wordsize wordsize))

        (emit port "mov rdx, [rsp - " sindex "]")
        (emit port "mov [rsp - " sindex "], rax")
        (emit port "shl rdx, 1")
        (emit port "add rdx, rax")
        (emit port "and rdx, ~0b010")
        (emit port "mov rax, [rsp - " (+ sindex wordsize) "]")
        (emit port "mov [rdx + 8], rax")
        (emit port "mov rax, [rsp - " sindex "]"))

       ((eq? op 'string)
        ;; (string ...) must have ... as a list of constant characters,
        ;; so there's no need to do an (emit-expr) here
        (let ((strlen (+ 1 (length (primcall-body x)))))
          ;; todo: find more efficient way to do this
          (define (str-iter x str)
            (if (not (null? str))
              (begin
                (emit port "mov al, " (char->integer (car str)))
                (emit port "mov [rbp + " x "], al")
                (str-iter (+ x 1) (cdr str)))

              (begin
                (emit port "mov al, 0")
                (emit port "mov [rbp + " x "], al"))))

          (emit port "mov rax, " strlen)
          (emit port "mov [rbp], rax")
          (str-iter wordsize (primcall-body x))

          (emit port "mov rax, rbp")
          (emit port "or rax, 0b011")
          (emit port "add rbp, " (+ wordsize strlen
                                    (- wordsize (modulo strlen wordsize))))))

      (true 'asdf))))

(define (emit-expr-list port xs sindex)
  (when (not (null? xs))
    (emit-comment port "emitting next expression in list")
    (emit-expr port (car xs) sindex)
    (emit-expr-list port (cdr xs) sindex)))

(define (emit-begin port x sindex)
  (emit-comment port (cdr x))
  (emit-expr-list port (cdr x) sindex))

(define bindings cadr)
(define body     cddr)

(define (emit-let port bindings body sindex)
  (define (f b* sindex)
    (emit-comment port b* " " sindex)
    (cond
      ((null? b*)
       (emit-expr-list port body sindex))

      (true
        (let ((b (car b*)))
          (emit-comment port "binding " (car b) " => " (cadr b))
          (emit-expr port (cadr b) sindex)
          (emit port "mov [rsp - " sindex "], rax")
          (f (cdr b*)
             (+ sindex 8))))))
  
  (f bindings sindex))

(define (emit-cmp port comp reg)
  (emit port "cmp " reg ", " comp))

(define (emit-je port labelspec)
  (emit port "je " labelspec))

(define (emit-jl port labelspec)
  (emit port "jl " labelspec))

(define (emit-jg port labelspec)
  (emit port "jg " labelspec))

(define (emit-jmp port labelspec)
  (emit port "jmp " labelspec))

(define (emit-if port test conseq altern sindex)
  (let ((L0 (unique-label))
        (L1 (unique-label)))
    (emit-expr port test sindex)
    (emit-cmp port (immediate-rep #f) 'rax)
    (emit-je port L0)

    (emit-expr port conseq sindex)
    (emit-jmp port L1)

    (emit-label port L0)
    (emit-expr port altern sindex)

    (emit-label port L1)))

(define (emit-closure port x sindex)
  (define (iter x cindex)
    (when (not (null? x))
      (emit-primitive-call port (car x) (+ wordsize sindex))
      (emit port "mov rdx, [rsp - " sindex "]")
      (emit port "mov [rdx + " (pointer-index (+ cindex 1)) "], rax")
      (iter (cdr x) (+ cindex 1))))

  (emit-comment port "making closure for " (cadr x)
                ", capturing " (cddr x))

  (emit port "mov rax, " (cadr x))
  (emit port "mov [rbp], rax")

  (emit port "mov [rsp - " sindex "], rbp")
  (emit port "add rbp, " (+ wordsize (* wordsize (length (cddr x)))))

  (iter (cddr x) 0)
  (emit port "mov rax, [rsp - " sindex "]")
  (emit port "or rax, 0b110"))

(define (emit-funcall port x sindex)
  (define new-stack-pos 3)
  
  (define (args-iter args i)
    (when (not (null? args))
      (emit-expr port (car args) (+ sindex (pointer-index i)))
      (emit port "mov [rsp - " (+ sindex (pointer-index i)) "], rax")
      (args-iter (cdr args) (+ i 1))))

  (args-iter (cdr x) new-stack-pos)

  (emit-comment port "operator: " (car x))
  (emit-expr port (car x) sindex)
  (emit port "mov rbx, rax")

  (emit-comment port "stack index: " sindex)
  (emit port "sub rsp, " sindex)
  (emit port "mov [rsp - 16], rbx")
  (emit port "and rbx, ~0b110")
  (emit port "mov rbx, [rbx]")
  (emit port "call rbx")
  (emit port "mov rbx, [rsp - 16]")
  (emit port "add rsp, " sindex))

(define (emit-foreign-call port x sindex)
  (define (iter args regs)
    (when (and (not (null? args))
               (not (null? regs)))
      (emit-expr port (car args) sindex)
      (emit-comment port "next arg register: " (cdr regs))
      (emit port "mov " (car regs) ", rax")
      (iter (cdr args) (cdr regs))))

  (iter (cddr x) call-regs)
  (emit-flag port "extern " (cadr x))
  (emit port "sub rsp, " sindex)
  (emit port "call " (cadr x))
  (emit port "add rsp, " sindex))

(define (gen-library-sym name thing)
  (string-concat (append (map (lambda (x)
                                (string-append (symbol->string x) "_"))
                              name)
                         (list thing))))

(define (emit-module-label port name thing)
  (emit-flag port
    (string-concat (append (map (lambda (x)
                                  (string-append (symbol->string x) "_"))
                                name)
                           (list thing ":")))))

(define (sanitize-module-sym sym)
  (define translate
    '((#\- "_hyphen_")
      (#\= "_equal_")
      (#\? "_question_")
      (#\< "_lessthan_")
      (#\> "_greaterthan_")
      (#\+ "_plus_")
      (#\- "_minus_")
      (#\* "_times_")
      (#\/ "_slash_")
      ))

  (define (iter xs)
    (cond
      ((null? xs) '())

      ((not (eq? (assq (car xs) translate) #f))
       (append (string->list (assq (car xs) translate))
               (iter (cdr xs))))

      (else (cons (car xs)
                  (iter (cdr xs))))))

  (list->string
    (iter (string->list (symbol->string sym)))))

(define (emit-library-set! port x sindex)
  (with x as (unused libname libsym value)
     (emit-comment port "library-set!: " libname " " libsym)
     (emit-expr port value sindex)
     (emit port "mov [" (gen-library-sym libname (sanitize-module-sym libsym)) "], rax")))

(define (emit-library-ref port x sindex)
  (with x as (unused libname libsym)
     (emit-comment port "library-ref: " libname " " libsym)
     (emit-flag port "extern " (gen-library-sym libname (sanitize-module-sym libsym)))
     (emit port "mov rax, [" (gen-library-sym libname (sanitize-module-sym libsym)) "]")))

(define (library-set!? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'library-set!)))

(define (library-ref? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'library-ref)))

(define (library-import? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'import)))

(define (gen-library-file-name libname)
  (gen-library-sym libname "lib"))

(define (emit-library-definition port x sindex)
  (emit-comment port "emitting library definition for " (library-name x))

  )

(define (emit-constant-set! port x sindex)
  (with x as (unused label value)
    (emit-comment port "constant-set!: " label ", "value)
    (emit-expr port value sindex)
    (emit port "mov [" label "], rax" )))

(define (emit-constant-ref port x)
  (with x as (unused label)
    (emit-comment port "constant-ref: " label)
    (emit port "mov rax, [" label "]")))

(define (emit-expr port x sindex)
  (cond
    ((immediate? x)
     (emit-comment port "immediate " x)
     (emit port "mov rax, " (immediate-rep x)))

    ((let? x)
     (emit-let port (bindings x) (body x) sindex))

    ((if? x)
     (emit-comment port "if " (cadr x))
     (emit-if port (cadr x) (caddr x) (caddr (cdr x)) sindex))

    ((variable? x)
     (emit-comment port "Got variable reference.")
     (emit-comment port "This shouldn't happen, should have been caught in var. ref pass" x)
     (emit-comment port "todo: error out here")
     (abandon-hope (list "Have unbound variable reference \"" x "\"in codegen, wut?")))

    ((primcall? x)
     (emit-primitive-call port x sindex))

    ((begin? x)
     (emit-begin port x sindex))

    ((closure? x)
     (emit-closure port x sindex))

    ((foreign-call? x)
     (emit-foreign-call port x sindex))

    ((library-definition? x)
     (emit-library-definition port x sindex))

    ((library-set!? x)
     (emit-library-set! port x sindex))

    ((library-ref? x)
     (emit-library-ref port x sindex))

    ((constant-set!? x)
     (emit-constant-set! port x sindex))

    ((constant-ref? x)
     (emit-constant-ref port x))

    ((list? x)
     (emit-funcall port x sindex))

    (true (print "wut"))))

;; code label -> code for the label
(define (label-code-body x)
  (cddr (cdadr x)))

;; code label -> free var. list
(define (label-code-free-vars x)
  (cadr (cadr x)))

(define (get-label-type x)
  (caadr x))

(define (code-label? x)
  (and (list? x)
       (not (null? x))
       (eq? (get-label-type x) 'code)))

(define (datum-label? x)
  (and (list? x)
       (not (null? x))
       (eq? (get-label-type x) 'datum)))

(define (emit-label-code port x labels)
  (emit-comment port (label-code-body x))
  (emit-label port (car x))
  (emit-expr-list port (label-code-body x)
                  (pointer-index (+ arg-stack-pos
                                    (length (label-code-free-vars x)))))
  (emit port "ret"))

(define (emit-label-datum port x labels)
  (emit-label port (car x))
  (emit port "resq 1"))

(define (emit-labels port x labels)
  (when (not (null? x))
    (let ((cur-label (car x)))
      (emit-comment port "emitting " cur-label)
      (cond
        ((code-label? cur-label)
         (emit-flag port "section .text")
         (emit-label-code port cur-label labels))

        ((datum-label? cur-label)
         (emit-flag port "section .bss")
         (emit-label-datum port cur-label labels))

        (else
          (abandon-hope (list "unknown label type :"
                              (get-label-type cur-label))))))

    (emit-labels port (cdr x) labels)))

(define (emit-program port x)
  (if (labels? x)
    (begin
      (emit-flag port "bits 64")
      (emit-flag port "extern scheme_heap")
      (emit-flag port "global scheme_thing")
      (emit-flag port "section .text")
      (emit-flag port "scheme_thing:")
      (emit port "push rbp")
      (emit port "push rbx")
      (emit port "mov rbp, scheme_heap")

      ; emit main program code
      (emit-flag port ".scheme_entry:")
      (emit-expr port (caddr x) (pointer-index arg-stack-pos))
      (emit port "pop rbx")
      (emit port "pop rbp")
      (emit port "ret")

      ; emit code for labels
      (emit-flag port ".scheme_labels:")
      (emit-labels port (cadr x) (cadr x)))

    (emit port "somethings wrong, expected a program with labels but got " x)))

(define (emit-library port x libname lib)
  (if (labels? x)
    (begin
      (emit-flag port "bits 64")

      (let ((exports    (get-lib-field 'export (library-fields lib)))
            (full-name  (library-name lib)))
        (emit-flag port "section .bss")

        (when (not (null? exports))
          (for sym in exports
               (emit-flag port "global " (gen-library-sym full-name (sanitize-module-sym sym)))
               (emit-module-label port full-name (sanitize-module-sym sym))
               (emit port "resq " 1))))

      (emit-flag port "section .text")
      (emit-flag port "global " libname)
      (emit-flag port libname ":")

      ; emit main library code
      (emit-expr port (caddr x) (pointer-index arg-stack-pos))
      (emit port "ret")

      ; emit code for labels
      (emit-flag port ".scheme_labels:")
      (emit-labels port (cadr x) (cadr x)))

    (emit port "somethings wrong, expected a program with labels but got " x)))

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
     (gen-free-vars (cddr x) defined-vars))

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

    ((quoted? x)
     '())

    ((begin? x)
     (gen-free-vars (cdr x) defined-vars))

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
  (define constants '())

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

  (define (gen-constant x)
    (let ((label (unique-temp-var)))
      (cond
        ((immediate? (cadr x))
         (cadr x))

        (else
          (set! constants
            (cons (list label (cadr x))
                  constants))

          (set! labels
            (cons
              (list label
                    (list 'datum))
              labels))

          (list 'constant-ref label)))))

  (define (replace-lambdas x)
    (cond
      ((lambda? x)
       (gen-closure x))

      ((quoted? x)
       (gen-constant x))

      ((null? x)
       '())

      ((list? x)
       (when (null? (car x))
         (abandon-hope (list "empty expression (todo: better error message)")))

       (cons
         (replace-lambdas (car x))
         (replace-lambdas (cdr x))))

      (true x)))

  (define (gen-constant-code label)
    (define (iter x)
      (cond
        ((null? x) '())

        ((expanded-string? x) x)

        ((list? x)
         (construct-cons
           (car x)
           (iter (cdr x))))

        (else x)))

    (list 'constant-set! (car label) (iter (cadr label))))

  (let ((new-body (replace-lambdas x)))
    (list 'labels labels
      (cons 'begin
        (append (map gen-constant-code constants)
                (list new-body))))))

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

    ((or (begin? x)
         (if? x))
     (cons (car x)
           (resolve-var-refs (cdr x) closure env)))

    ((primcall? x)
     (cons (car x)
       (cons (cadr x)
         (resolve-var-refs (cddr x) closure env))))

    ((foreign-call? x)
     (cons 'foreign-call
       (cons (cadr x)
         (resolve-var-refs (cddr x) closure env))))

    ((variable? x)
     (cond
       ((member? x env)
        (construct-primitive-call
          (list 'stack-ref (list-index x env))))

       ((member? x (closure-free closure))
        (construct-primitive-call
          (list 'closure-ref (list-index x (closure-free closure)))))

       (true
         (abandon-hope (list "undefined variable :" x))
         ;(emit-comment "didn't find " x ". (todo: error out here)")
         'unfound-variable)))

    ((library-definition? x)
     (construct-library-definition
       (cadr x)
       (change-lib-field
         'begin
         (resolve-var-refs (get-lib-field 'begin (library-fields x)) '() '())
         (library-fields x))))

    ((or (library-set!?  x)
         (constant-set!? x))
     (cons
       (car x)
       (cons
         (cadr x)
         (cons
           (caddr x)
           (resolve-var-refs (cddr (cdr x)) closure env)))))

    ((or (library-ref?  x)
         (constant-ref? x))
     x)

    ((list? x)
     (cons
       (resolve-var-refs (car x) closure env)
       (resolve-var-refs (cdr x) closure env)))

    (true x)))

(define (resolve-code-label-var-refs x)
  (if (code-label? x)
    (let ((code-def (cadr x)))
      (list (car x)
            (list (car code-def)
                  (cadr code-def)
                  (caddr code-def)
                  (apply values
                         (resolve-var-refs (label-code-body x)
                                           code-def
                                           (cadr code-def))))))
   else x))

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

(define (construct-primitive-call x)
  (cons 'primitive-call x))

(define (construct-lambda args body)
  (cons 'lambda
    (cons args body)))

(define (construct-let binds body)
  (cons 'let
    (cons binds body)))

(define (construct-vector :rest args)
  (construct-primitive-call
    (cons 'vector args)))

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

(define (construct-assignment-set vec index value)
  (construct-primitive-call
    (cons 'assignment-set!
      (cons vec
        (cons index
          (cons value '()))))))

(define (construct-assignment-ref vec index)
  (construct-primitive-call
    (cons 'assignment-ref
      (cons vec
        (cons index '())))))

(define (construct-if condition path1 path2)
  (list 'if condition path1 path2))

(define (construct-begin :rest code)
  (cons 'begin code))

(define (construct-cons foo bar)
  (construct-primitive-call (list 'cons foo bar)))

(define set!-op-1 cadr)
(define set!-op-2 caddr)

(define (change-let-binding name new-value binds)
  (cond
    ((null? binds)
     '())

    ((eq? name (caar binds))
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

    ((or (assignment-set!? x)
         (assignment-ref?  x))
     x)

    ((library-ref? x) x)

    ((primcall? x)
     (construct-primitive-call
       (cons (primcall-op x)
             (replace-assignments (primcall-body x) vars))))

    ((and (variable? x)
          (member? x vars))
     (construct-assignment-ref x 0))

    ((set!? x)
     (construct-assignment-set
       (set!-op-1 x)
       0
       (replace-assignments (set!-op-2 x) vars)))

    ((list? x)
      (cons
        (replace-assignments (car x) vars)
        (replace-assignments (cdr x) vars)))

    (true x)))

(define (insert-lets vars xs)
  ;(emit-comment "replacing " vars)
  (list (construct-let
      (list (list (car vars)
                  (construct-vector (cadr vars))))
      xs)))

(define (transform-assignments x)
  (cond
    ((null? x)
     (list '() '()))

    ((library-ref? x) (list '() x))

    ((lambda? x)
     (let* ((assigns    (transform-assignments (lambda-body x)))
            (found-vars (intersect (lambda-args x) (car assigns)))
            (var-pairs  (map (lambda (x)
                               (list x (unique-temp-var)))
                             found-vars)))

       ;assigns
       (list
         (list-remove-objs (lambda-args x) (car assigns))
         (construct-lambda
           (list-replace-objs* var-pairs (lambda-args x))
           (expand insert-lets
                   var-pairs
                   (replace-assignments (cadr assigns) (car assigns)))))))

    ((let? x)
     (let* ((left  (transform-assignments (bindings x)))
            (right (transform-assignments (body x)))
            (assigns (append (car left) (car right)))
            (found-vars (intersect (map car (bindings x)) assigns)))

       (list
         (list-remove-objs (map car (bindings x)) assigns)
         (construct-let
           (expand vectorise-mutable-bindings
                   found-vars
                   (cadr left))
           ;(cadr left)
           (replace-assignments (cadr right) found-vars)))))

    ((set!? x)
     (let ((set-body (transform-assignments (set!-op-2 x))))
       (list (list (set!-op-1 x))
             (cons 'set!
                   (cons
                     (set!-op-1 x)
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

(define (expand-import x body)
  (with x as (unused libname)
    (let* ((stubfile (open (find-library-stub-path libname) "r"))
           (lib      (read stubfile))
           (exports  (get-lib-field 'export (library-fields lib))))

      (cons
        (list 'foreign-call (gen-library-sym libname "lib"))

        (append (map (lambda (sym)
                       (list 'define sym (list 'library-ref libname sym)))
                     exports)
                body)))))

(define (expand-library-definition x)
  (let ((exports   (get-lib-field 'export (library-fields x)))
        (libbody   (get-lib-field 'begin  (library-fields x))))

    (cons
      'begin
      (append libbody
              (map (lambda (sym)
                     (list 'library-set! (library-name x) sym sym))
                   exports))
      (library-fields x))))

(define (library-definition-expanded? x)
  (eq? (get-lib-field 'expanded (library-fields x)) #t))

(define (rewrite-core-syntax x)
  (cond
    ((null? x)
     '())

    ((and (list? x)
          (define? (car x)))
     (rewrite-core-syntax
       (expand-define (car x) (cdr x))))

    ((and (list? x)
          (library-import? (car x)))
     (rewrite-core-syntax
       (expand-import (car x) (cdr x))))

    ((unexpanded-library-definition? x)
     (rewrite-core-syntax
       (expand-library-definition x)))

    ((cond? x)
     (rewrite-core-syntax (expand-cond x)))

    ((list? x)
      (cons
        (rewrite-core-syntax (car x))
        (rewrite-core-syntax (cdr x))))

    (true x)))

(define (expand-string x)
  (construct-primitive-call
    (cons 'string (string->list x))))

(define (expand-string-constants x)
  (cond
    ((null? x) '())

    ((foreign-call? x) x)

    ((string? x)
     (expand-string x))

    ((list? x)
     (cons (expand-string-constants (car x))
           (expand-string-constants (cdr x))))

    (else x)))

(load! "pretty.scm")

(define (get-base-filename str)
  (list->string
    (reverse (after (reverse (string->list str)) #\.))))

(define (get-base-directory str)
  (let ((after-slash (after (reverse (string->list str)) #\/)))
    (if (null? after-slash)
      "."
      (list->string (reverse after-slash)))))

(define (do-transform-assignments x)
  (let ((tranformed (transform-assignments x)))
    (assert null? (car tranformed)
            "undefined/out of scope references in set!")
    tranformed))

(define (analysis-passes x dumpflags)
  (define (dump arg value)
    (when (and (not (eq? dumpflags #f))
               (member? arg dumpflags))
      (pretty value))
    value)

  (dump "resolved" (resolve-labels-var-refs
  (dump "labeled"    (gen-labels
  (dump "lambdas"      (transform-lambdas
  (dump "assigned"       (cadr (do-transform-assignments
  (dump "expanded"         (rewrite-core-syntax
                                (expand-string-constants x))))))
                       '())))))))

;; find library definitions at the top level
(define (find-libraries x)
  (define found-libs '())

  (define (recurse code)
  (cond
    ((null? code)
     '())

    ((and (list? code)
         (unexpanded-library-definition? (car code)))
     (set! found-libs (cons (car code) found-libs))
     (recurse (cdr code)))

    ((list? code)
      (cons (car code)
            (recurse (cdr code))))

    (else code)))

  (let ((new-body (recurse x)))
    (list found-libs new-body)))

(define (find-imports x)
  (cond
    ((null? x)
     '())

    ((and (list? x)
          (library-import? (car x)))
     (append (cdar x)
             (find-imports (cdr x))))

    ((list? x)
     (find-imports (cdr x)))

    (else '())))

(define (compile-program filename args code)
  (let* ((base-name (get-base-filename filename))
         (asm-name  (string-append base-name ".asm"))
         (obj-name  (string-append base-name ".o"))
         (prog-name base-name)
         (imports   (find-imports code)))

    (for-each display (list "compiling " filename #\newline))
    (for-each display (list #\tab " => " asm-name  #\newline
                            #\tab " => " obj-name  #\newline
                            #\tab " => " prog-name #\newline))

    (for import in imports
         (display-list "import: " import " => " (gen-library-file-name import) ".o" #\newline))

    (cond
      ((arguments-contain-flag? "-dump" args)
       (let ((dumps (get-lib-field "-dump" args)))
         (analysis-passes code dumps)))

      (else
        (let ((out-port (open asm-name "w")))
          (emit-program out-port
            (analysis-passes (cons 'begin code) #f))
          (close out-port))

        (system (concat "nasm -felf64 -o " obj-name " " asm-name))
        (system (string-concat
                  (append
                    (list "gcc -o " prog-name " " obj-name " stub.o")
                    (map (lambda (x)
                           (string-append
                             " "
                             (find-library-object-path x)))
                         imports))))))))

(define (compile-library-stub lib filename)
  (let ((stubfile (open filename "w")))
    (write
      (construct-library-definition
        (library-name lib)
        (change-lib-field 'begin '(...) (library-fields lib)))
      stubfile)

    (display #\newline stubfile)
    (close stubfile)))

(define (compile-library filename args lib)
  (let* ((base-dir  (get-base-directory filename))
         (lib-name  (gen-library-file-name (library-name lib)))
         (asm-name  (concat base-dir "/" lib-name ".asm"))
         (stub-name (concat base-dir "/" lib-name ".libstub"))
         (obj-name  (concat base-dir "/" lib-name ".o")))

    (for-each display (list "got library " (library-name lib)
                            #\newline #\tab " => " asm-name
                            #\newline #\tab " => " obj-name
                            #\newline #\tab " => " stub-name
                            #\newline
                            ))

    (cond
      ((arguments-contain-flag? "-dump" args)
       (let ((dumps (get-lib-field "-dump" args)))
         (analysis-passes lib dumps)))

      (true
        (compile-library-stub lib stub-name)

        (let ((out-port (open asm-name "w")))
          (emit-library out-port
                        (analysis-passes lib #f)
                        lib-name
                        lib)
          (close out-port))

        (system (concat "nasm -f elf64 -o " obj-name " " asm-name))))))

(define (compile-file filename args)
  (define program (read-program (open filename "r")))

  (with (find-libraries program) as (libs code)
        (for lib in libs
             (let ((basedir   (get-base-directory filename))
                   (libname   (get-base-filename (gen-library-file-name (library-name lib)))))

               (compile-library filename args lib)))

        (if (null? code)
          (for-each display (list "file " filename " has no code statements, not compiling..." #\newline))
          (compile-program filename args code))))

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
    (for filename in (argument-fields args)
        (compile-file filename args))

  else
    (for-each print
      '("usage: ./foo.scm [options] program.scm"
        "  options:"
        "    -library             : Specify that [program.scm] is a library."
        "                           this prevents the compiler from outputting"
        "                           various entry-point code that would interfere"
        "                           with linking."
        "    -dump:[ expanded     : Output stages of the analysis pass for debugging."
        "          | assigned       multiple dumps can be specified by seperating"
        "          | lambdas        options with a colon, eg. '-dump:expanded:labeled'"
        "          | labeled"
        "          | resolved ]"))))
