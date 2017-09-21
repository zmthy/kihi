#lang racket

(require (for-syntax racket)
         "program.rkt"
         "runtime.rkt")

(provide (for-syntax begin-syntax))

(begin-for-syntax

  (define (begin-syntax stx)
    (expand-form stx #'run))

  (define (expand-form stx over)
    (let-values ([(def-forms expr-forms) (expand-all (syntax-e stx))])
      (with-syntax ([(def-forms ...) def-forms]
                    [(expr-forms ...) expr-forms]
                    [over over])
        #'(def-forms ... (over expr-forms ...)))))

  (define (lookup-expand form)
    (case (syntax-e form)
      ['define (values expand-define cons-left)]
      ['bind (values expand-bind cons-right)]
      ['let (values expand-let cons-right)]
      [else
       (values
        (λ ()
          (syntax-case form ()
            [(forms ...)
             (with-syntax ([(body ...) (expand-form form #'program)])
               #`(λ () body ...))]
            [else form]))
        cons-right)]))

  (define (cons-left x a b)
    (values (cons x a) b))

  (define (cons-right x a b)
    (values a (cons x b)))

  (define (expand-all forms)
    (match forms
      [(cons form forms)
       (let*-values ([(expand-form output-form) (lookup-expand form)]
                     [(arity) (procedure-arity expand-form)]
                     [(args forms) (split-at forms arity)]
                     [(form) (apply expand-form args)]
                     [(def-forms expr-forms) (expand-all forms)])
         (output-form form def-forms expr-forms))]
      [(list)
       (values empty empty)]))

  (define (expand-define f p)
    (with-syntax ([(f x ...) f]
                  [(p ...) p])
      #'(define (f x ...) (run p ...))))

  (define (expand-bind b p)
    (with-syntax ([(x ...) b]
                  [(p ...) p])
      #'(λ (x ...) (run p ...))))

  (define (expand-let b p)
    (with-syntax ([r (expand-let* b p)])
      #'(λ () r)))

  (define (expand-let* b p)
    (syntax-case b ()
      [() (with-syntax ([(p ...) p])
            #'(run p ...))]
      [([(x ...) t ...] b ...)
       (with-syntax ([(p ...) (expand-let #'(b ...) p)])
         #'(run (λ (x ...) (p ...)) t ...))])))
