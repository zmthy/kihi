#lang racket

(require "private/program.rkt"
         "private/stream.rkt"
         "lib/primitive.rkt"
         "lib/stack.rkt"
         "lib/program.rkt"
         "lib/boolean.rkt"
         "lib/number.rkt"
         "lib/string.rkt"
         "lib/pair.rkt"
         "lib/option.rkt"
         "lib/list.rkt")

(provide (rename-out [module-begin #%module-begin]
                     [app #%app])
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

(define-syntax-rule (module-begin forms ...)
  (#%module-begin (stream (list forms ...))))

(define-syntax-rule (app forms ...)
  (program forms ...))
