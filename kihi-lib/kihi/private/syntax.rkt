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
    (wrap-form #'(forms ...) #'#%module-begin #'run-print)))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ forms ...) (begin-print #'(forms ...))]
    [(_ . form) #'form]))

(begin-for-syntax

  (define (begin-values stx)
    (wrap-form stx #'begin #'run-values))

  (define (begin-print stx)
    (wrap-form stx #'begin #'run-print))

  (define (begin-stream stx)
    (wrap-form stx #'begin #'run-stream))

  (define (thunk-program stx)
    (wrap-form stx #'thunk 'program))

  (define (wrap-form stx defs exprs)
    (let-values ([(def-forms expr-forms) (expand-form stx)])
      #`(#,defs #,@def-forms (#,exprs #,@expr-forms))))

  (define (lookup-expand form)
    (case (syntax-e form)
      ['racket (values expand-racket cons-right)]
      ['racket/stmt (values expand-racket cons-left)]
      ['require (values expand-require cons-left)]
      ['provide (values expand-provide cons-left)]
      ['define (values expand-define cons-left)]
      ['struct (values expand-struct cons-left)]
      ['bind (values expand-bind cons-right)]
      ['let (values expand-let cons-right)]
      ['λ (values expand-lambda cons-right)]
      ['match (values expand-match cons-right)]
      ['arity (values expand-arity cons-right)]
      [else
       (values (expand-expr form) cons-right)]))

  (define (cons-left x a b)
    (values (cons x a) b))

  (define (cons-right x a b)
    (values a (cons x b)))

  (define (expand-form stx)
    (expand-all (syntax-e stx)))

  (define (expand-all forms)
    (match forms
      [(cons form forms)
       (let*-values ([(consume-forms output-form) (lookup-expand form)]
                     [(arity) (procedure-arity consume-forms)]
                     [(args forms remaining) (split-at forms (sub1 arity))]
                     [(context) (datum->syntax form (cons form args))]
                     [(form) (if (zero? remaining)
                                 (apply consume-forms context args)
                                 (raise-syntax-error
                                  #f
                                  (format "not enough forms: expected ~a more"
                                          remaining)
                                  context))]
                     [(def-forms expr-forms) (expand-all forms)])
         (output-form form def-forms expr-forms))]
      [(list)
       (values empty empty)]))

  (define ((expand-expr form) ctx)
    (syntax-parse form
      #:context ctx
      [(_ ...) (thunk-program form)]
      [else form]))

  (define (expand-racket ctx e)
    (syntax-parse e
      #:context ctx
      [(t ...) #'(t ...)]))

  (define (expand-require ctx s)
    (syntax-parse s
      #:context ctx
      [(s ...) #`(require #,@(slurp-left #'(s ...) #'(s ...)))]))

  (define (expand-provide ctx s)
    (syntax-parse s
      #:context ctx
      [(s ...) #`(provide #,@(slurp-left #'(s ...) #'(s ...)))]))

  (define (slurp-left ctx s)
    (syntax-parse s
      #:context ctx
      [((f ...) s ...) #'((f ...) s ...)]
      [(x:id (n ...) s ...) #`((x #,@(slurp-left #'x #'(n ...)))
                               #,@(slurp-left ctx #'(s ...)))]
      [(f s ...) #`(f #,@(slurp-left ctx #'(s ...)))]
      [() #'()]))

  (define (expand-define ctx s p)
    (syntax-parse #`[#,s #,p]
      #:context ctx
      [[(f:id) (_ ...)]
       #`(define f
           (program-thunk #,(thunk-program p)))]
      [[(f:id x:id ...) (_ ...)]
       (let-values ([(def-forms expr-forms) (expand-form p)])
         #`(define (f x ... . args)
             #,@def-forms
             (apply run-values #,@expr-forms args)))]))

  (define (expand-struct ctx s)
    (syntax-parse s
      #:context ctx
      [(x:id y:id ...) #`(struct x (y ...))]))

  (define (expand-bind ctx b p)
    (syntax-parse #`[#,b #,p]
      #:context ctx
      [((x:id ...) (_ ...))
       (let-values ([(def-forms expr-forms) (expand-form p)])
         #`(λ (x ...)
             #,@def-forms
             (execute (program #,@expr-forms))))]))

  (define (expand-lambda ctx b p)
    #`(thunk #,(expand-bind ctx b p)))

  (define (expand-let ctx b p)
    (syntax-parse b
      #:context ctx
      [() (syntax-parse p
            #:context ctx
            [(t ...) #'(execute (program t ...))])]
      [([(x:id ...) t ...] b ...)
       #`(execute (program (λ (x ...)
                             #,(expand-let ctx #'(b ...) p))
                           t ...))]))

  (define (expand-match ctx c)
    (syntax-parse c
      #:context ctx
      [(_ ...)
       #`(match-lambda #,@(syntax-map ctx expand-match* c))]))

  (define (expand-match* ctx c)
    (syntax-parse c
      #:context ctx
      [[m t ...]
       (wrap-form #'(t ...) #'m #'run-values)]))

  (define (expand-arity ctx n t)
    (syntax-parse t
      #:context ctx
      [(t) #`(procedure-reduce-arity t #,n)]))

  (define (syntax-map ctx f stx)
    (datum->syntax stx (map (curry f ctx) (syntax->list stx))))

  (define (split-at lst pos)
    (split empty lst pos))

  (define (split fst lst pos)
    (if (or (zero? pos)
            (empty? lst))
        (values (reverse fst) lst pos)
        (split (cons (first lst) fst) (rest lst) (sub1 pos)))))
