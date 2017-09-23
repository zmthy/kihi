#lang racket

(require (for-syntax (except-in racket
                                split-at)
                     syntax/parse
                     (only-in "split.rkt"
                              split-at))
         "program.rkt"
         "runtime.rkt")

(provide (for-syntax begin-syntax))

(begin-for-syntax

  (define (begin-syntax stx)
    (expand-form stx #'run))

  (define (expand-form stx over)
    (let-values ([(def-forms expr-forms) (expand-all (syntax-e stx))])
      #`(#,@def-forms (#,over #,@expr-forms))))

  (define (lookup-expand form)
    (case (syntax-e form)
      ['require (values expand-require cons-left)]
      ['provide (values expand-provide cons-left)]
      ['define (values expand-define cons-left)]
      ['bind (values expand-bind cons-right)]
      ['let (values expand-let cons-right)]
      [else
       (values (expand-expr form) cons-right)]))

  (define (cons-left x a b)
    (values (cons x a) b))

  (define (cons-right x a b)
    (values a (cons x b)))

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
      [(forms ...) #`(thunk #,@(expand-form form #'program))]
      [else form]))

  (define (expand-require ctx s)
    (syntax-parse s
      #:context ctx
      [(s ...) #'(require s ...)]))

  (define (expand-provide ctx s)
    (syntax-parse s
      #:context ctx
      [(s ...) #'(provide s ...)]))

  (define (expand-define ctx s p)
    (syntax-parse s
      #:context ctx
      [(f:id x:id ...) (syntax-parse p
                         #:context ctx
                         [(t ...)
                          #'(define (f x ...) (run t ...))])]))

  (define (expand-bind ctx b p)
    (syntax-parse b
      #:context ctx
      [(x:id ...) (syntax-parse p
                    #:context ctx
                    [(t ...)
                     #'(λ (x ...) (run t ...))])]))

  (define (expand-let ctx b p)
    #`(thunk #,(expand-let* ctx b p)))

  (define (expand-let* ctx b p)
    (syntax-parse b
      #:context ctx
      [() (syntax-parse p
            #:context ctx
            [(t ...) #'(run t ...)])]
      [([(x:id ...) t ...] b ...)
       #`(run (λ (x ...) #,(expand-let #'(b ...) p)) t ...)])))
