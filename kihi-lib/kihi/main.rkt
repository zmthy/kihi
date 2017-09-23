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
         only-in
         except-in
         prefix-in
         rename-in
         relative-in
         all-defined-out
         all-from-out
         rename-out
         except-out
         prefix-out
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
  (with-syntax ([(_ forms ...) stx])
    #`(#%module-begin #,@(begin-syntax #'(forms ...)))))
