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
      ['let (values expand-let cons-right)]
      ['λ (values expand-lambda cons-right)]
      ['match (values expand-match cons-right)]
      ['with-arity (values expand-with-arity cons-right)]
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
            [(args forms remaining)
             (if (arity-at-least? arity)
                 (values forms '() 0)
                 (split-at forms arity))]
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
      [(_ ...)
       (let-values ([(defs exprs) (expand-form form)])
         #`(thunk #,@defs (program #,@exprs)))]
      [else form]))

  (define (expand-with-arity n ref)
    (parse ref
      [f:id #`(execute (procedure-reduce-arity f #,n))]))

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

  ;; A binder is a bare name, which binds a value, or a name in parentheses,
  ;; which binds a procedure that is executed when the name is used.  A list
  ;; of binders is written in square brackets.
  (define (square? stx)
    (eqv? (syntax-property stx 'paren-shape) #\[))

  (define (binder? stx)
    (or (identifier? stx)
        (and (not (square? stx))
             (parse stx
               [(x:id) #t]
               [_ #f]))))

  (define ((check-binder who) stx)
    (if (binder? stx)
        stx
        (raise-syntax-error
         who "expected a name or a name in parentheses" stx)))

  (define (binders who stx)
    (cond
      [(square? stx) (map (check-binder who) (syntax->list stx))]
      [(binder? stx) (list stx)]
      [else
       (raise-syntax-error
        who "expected a name, a name in parentheses, or names in square brackets"
        stx)]))

  ;; A procedure signature is a name applied to one or more binders.
  (define (signature? stx)
    (and (not (square? stx))
         (parse stx
           [(f:id b ...+) #t]
           [_ #f])))

  (define (flatten-binding binding)
    (parse binding
      [(x:id) #'x]
      [x:id #'x]))

  (define (bind-value binding value)
    (if (identifier? binding)
        #`(no-execute #,value)
        value))

  (define (no-execute-binding binding bindings)
    (if (identifier? binding)
        (cons #`[#,binding #,(bind-value binding binding)] bindings)
        bindings))

  (define (no-execute-bindings bindings)
    (foldr no-execute-binding empty bindings))

  (define (expand-procedure who bindings body)
    (unless (syntax->list body)
      (raise-syntax-error who "expected a parenthesized procedure body" body))
    (let-values ([(def-forms expr-forms) (expand-form body)])
      #`(λ (#,@(map flatten-binding bindings) . args)
          (let #,(no-execute-bindings bindings)
            #,@def-forms
            (apply run-values #,@expr-forms args)))))

  (define (expand-define bindings body)
    (parse bindings
      [f:id
       (with-context bindings
         (parse body
           [(name:id) #`(define f (no-execute name))]
           [_ #`(define f #,((expand-expr body)))]))]
      [(f:id b ...)
       #:when (not (square? bindings))
       (let ([procedure (expand-procedure
                         'define
                         (map (check-binder 'define) (syntax->list #'(b ...)))
                         body)])
         (with-context bindings
           #`(define f #,procedure)))]
      [_
       (raise-syntax-error
        'define "expected a name or a parenthesized procedure signature" bindings)]))

  (define (expand-struct bindings)
    (parse bindings
      [(x:id y:id ...) #'(struct x (y ...))]))

  (define (expand-lambda bindings body)
    (let ([params (binders 'λ bindings)])
      (unless (syntax->list body)
        (raise-syntax-error 'λ "expected a parenthesized procedure body" body))
      (let-values ([(def-forms expr-forms) (expand-form body)])
        #`(thunk
            (λ #,(map flatten-binding params)
              (let #,(no-execute-bindings params)
                #,@def-forms
                (execute (program #,@expr-forms))))))))

  (define (expand-let bindings . body-forms)
    (if (signature? bindings)
        (expand-let-procedure bindings body-forms)
        (expand-let-values (binders 'let bindings) body-forms)))

  ;; Bind a local procedure, in scope for its own body and the remaining terms.
  (define (expand-let-procedure signature body-forms)
    (when (empty? body-forms)
      (raise-syntax-error 'let "expected a procedure body" signature))
    (parse signature
      [(f:id b ...)
       (let*-values ([(procedure)
                      (expand-procedure 'let
                                        (map (check-binder 'let) (syntax->list #'(b ...)))
                                        (first body-forms))]
                     [(defs exprs) (expand-all (rest body-forms))])
         #`(thunk
            (letrec ([f #,procedure])
              #,@defs
              (apply values
                (stream->list (run-forms #,(expr-stream exprs)))))))]))

  ;; Bind successive values produced by the terms, which are evaluated with
  ;; the names in scope.
  (define (expand-let-values names body-forms)
    (let-values ([(defs exprs) (expand-all body-forms)])
      (if (empty? exprs)
          #`(thunk #,@defs (void))
          #`(thunk
             (letrec #,(map (λ (n) #`[#,(flatten-binding n) (void)]) names)
               #,@defs
               (apply values
                 (stream->list
                   (run-forms
                     (stream-cons
                       #,(foldr (λ (n acc)
                                  #`(execute
                                     (λ (v)
                                       (set! #,(flatten-binding n) #,(bind-value n #'v))
                                       #,acc)))
                                #'(void)
                                names)
                       #,(expr-stream exprs))))))))))

  (define (expr-stream exprs)
    (foldr (λ (expr acc)
             #`(stream-cons (execute-if-procedure #,expr) #,acc))
           #'empty-stream
           exprs))

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
