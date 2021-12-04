#lang racket

(require (for-syntax racket
                     syntax/parse)
         kihi/private/runtime)

(provide module-begin
         top-interaction
         (for-syntax begin-values
                     begin-stream))

(define-syntax (module-begin stx)
  (with-syntax ([(_ forms ...) stx])
    (let-values ([(defs exprs) (expand-form #'(forms ...))])
      #`(#%module-begin #,@defs (run-print #,@exprs)))))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ forms ...) (begin-print #'(forms ...))]
    [(_ . form) #'form]))

(begin-for-syntax

  (define context
    (make-parameter #f))

  (define-syntax-rule (with-context expr form)
    (parameterize ([context expr])
      form))

  (define-syntax-rule (parse form body ...)
    (syntax-parse form body ...))

  (define (begin-values form)
    (run-with #'run-values form))

  (define (begin-print form)
    (run-with #'run-print form))

  (define (begin-stream form)
    (run-with #'run-stream form))

  (define (run-with run form)
    (let-values ([(defs exprs) (expand-form form)])
      #`(begin #,@defs (#,run #,@exprs))))

  (define (lookup-expand form)
    (case (syntax-e form)
      ['racket (values expand-racket cons-right)]
      ['racket/stmt (values expand-racket-direct cons-left)]
      ['require (values expand-require cons-left)]
      ['provide (values expand-provide cons-left)]
      ['define (values expand-define cons-left)]
      ['struct (values expand-struct cons-left)]
      ['bind (values expand-bind cons-right)]
      ['let (values expand-let cons-right)]
      ['λ (values expand-lambda cons-right)]
      ['match (values expand-match cons-right)]
      [else (values (expand-expr form) cons-right)]))

  (define (cons-left x a b)
    (values (cons x a) b))

  (define (cons-right x a b)
    (values a (cons x b)))

  (define (expand-form form)
    (expand-all (syntax-e form)))

  (define (expand-all forms)
    (match forms
      [(cons form forms)
       (let*-values
           ([(consume-forms output-form) (lookup-expand form)]
            [(arity) (procedure-arity consume-forms)]
            [(args forms remaining) (split-at forms arity)]
            [(context) (datum->syntax form (cons form args))]
            [(form)
             (if (zero? remaining)
                 (with-context context (apply consume-forms args))
                 (raise-syntax-error
                  #f
                  (format "not enough forms: expected ~a more" remaining)
                  context))]
            [(def-forms expr-forms) (expand-all forms)])
         (output-form form def-forms expr-forms))]
      [(list)
       (values empty empty)]))

  (define ((expand-expr form))
    (parse form
      [(form) #'(no-execute form)]
      [(_ ...)
       (let-values ([(defs exprs) (expand-form form)])
         #`(λ ()
             #,@defs
             (program #,@exprs)))]
      [else form]))

  (define (expand-racket-direct form)
    (parse form
      [(t ...) #'(t ...)]))

  (define (expand-racket form)
    #`(thunk #,(expand-racket-direct form)))

  (define (expand-require form)
    (parse form
      [(s ...) (with-context form
                 #`(require #,@(slurp-left #'(s ...))))]))

  (define (expand-provide form)
    (parse form
      [(s ...) (with-context form
                 #`(provide #,@(slurp-left #'(s ...))))]))

  (define (slurp-left form)
    (parse form
      [((f ...) s ...) #'((f ...) s ...)]
      [(x:id (n ...) s ...) #`((x #,@(with-context #'x (slurp-left #'(n ...))))
                               #,@(slurp-left #'(s ...)))]
      [(f s ...) #`(f #,@(slurp-left #'(s ...)))]
      [() #'()]))

  (define (flatten-binding binding)
    (parse binding
      [(x:id) #'x]
      [x:id #'x]))

  (define (no-execute-binding binding bindings)
    (parse binding
      [(x:id) bindings]
      [x:id (cons #'[x (no-execute x)] bindings)]))

  (define (no-execute-bindings bindings)
    (with-syntax-list (curry foldr no-execute-binding empty) bindings))

  (define (expand-define bindings body)
    (parse bindings
      [(f:id b ...)
       (let-values ([(def-forms expr-forms) (expand-form body)])
         (with-context bindings
           #`(define
               (f #,@(syntax-map flatten-binding #'(b ...)) . args)
               (let #,(no-execute-bindings #'(b ...))
                 #,@def-forms
                 (apply run-values #,@expr-forms args)))))]))

  (define (expand-struct bindings)
    (parse bindings
      [(x:id y:id ...) #'(struct x (y ...))]))

  (define (expand-bind bindings body)
    (parse #`[#,bindings #,body]
      [[(x:id ...) (_ ...)]
       (let-values ([(def-forms expr-forms) (expand-form body)])
         #`(λ #,(syntax-map flatten-binding #'(x ...))
             (let #,(no-execute-bindings #'(x ...))
               #,@def-forms
               (execute (program #,@expr-forms)))))]))

  (define (expand-lambda bindings body)
    #`(thunk #,(expand-bind bindings body)))

  (define (expand-let bindings body)
    (parse bindings
      [() (parse body
            [(t ...) #'(execute (program t ...))])]
      [([(x:id ...) t ...] b ...)
       #`(execute (program (λ (x ...)
                             #,(expand-let #'(b ...) body))
                           t ...))]))

  (define (expand-match form)
    (parse form
      [(_ ...)
       #`(match-lambda #,@(syntax-map expand-case form))]))

  (define (expand-case form)
    (parse form
      [[m t ...]
       (let-values ([(defs exprs) (expand-form #'(t ...))])
         #`[m #,@defs (run-values #,@exprs)])]))

  (define (with-syntax-list f stx)
    (datum->syntax stx (f (syntax->list stx))))

  (define (syntax-map f stx)
    (with-syntax-list (curry map f) stx))

  (define (split-at lst pos)
    (split empty lst pos))

  (define (split fst lst pos)
    (if (or (zero? pos)
            (empty? lst))
        (values (reverse fst) lst pos)
        (split (cons (first lst) fst) (rest lst) (sub1 pos)))))
