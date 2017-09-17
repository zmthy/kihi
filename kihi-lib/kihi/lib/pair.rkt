#lang racket

(require "primitive.rkt"
         "stack.rkt"
         "program.rkt")

(provide (rename-out [cons pair])
         pair?
         (contract-out (rename car first (-> pair? any/c))
                       (rename cdr second (-> pair? any/c))
                       [pair/fold (-> program? pair? any)]
                       [pair/map (-> program? program? pair? any)]))

(define (unpair x)
  (match x
    [(cons y z) (values y z)]))

(define (pair/fold f x)
  (match x
    [(cons y z) (values apply f y z)]))

(define (pair/map f g x)
  (pair/fold (program cons apply f under g) x))
