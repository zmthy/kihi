#lang racket

(require (prefix-in racket/ racket)
         "../private/program.rkt"
         "primitive.rkt")

(provide (rename-out [empty nil]
                     [make-list repeat])
         (contract-out [cons (-> any/c list? list?)]
                       [list/fold (-> program? list? any)]
                       [list/map (-> program? list? any)]
                       (rename append list/append (-> list? list? list?))))

(define (list/fold f x)
  (racket/apply values (foldr (λ (v s) (append (program apply f v) s))
                              empty x)))

(define (list/map f x)
  (list/fold (program cons apply f) x empty))
