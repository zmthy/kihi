#lang racket

(require "private/syntax.rkt"
         "private/program.rkt"
         "private/runtime.rkt"
         "lib/primitive.rkt"
         "lib/stack.rkt"
         "lib/program.rkt"
         "lib/boolean.rkt"
         "lib/number.rkt"
         "lib/string.rkt"
         "lib/pair.rkt"
         "lib/option.rkt"
         "lib/list.rkt")

(provide (rename-out [module-begin #%module-begin])
         #%datum
         #%top
         #%top-interaction
         (all-from-out "lib/primitive.rkt"
                       "lib/stack.rkt"
                       "lib/program.rkt"
                       "lib/boolean.rkt"
                       "lib/number.rkt"
                       "lib/string.rkt"
                       "lib/pair.rkt"
                       "lib/option.rkt"
                       "lib/list.rkt"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (with-syntax ([(forms ...) (begin-syntax #'(forms ...))])
       #'(#%module-begin forms ...))]))
