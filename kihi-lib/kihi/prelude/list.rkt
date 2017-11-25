#lang kihi/base

require (prefix-in (racket:
                    racket)
         kihi/prelude/primitive
         kihi/prelude/stack
         kihi/prelude/number
         racket/contract)

provide (list?
         list
         cons
         rename-out ([racket:empty nil]
                     [racket:make-list repeat])
         contract-out ([rename fold list/fold
                               (->* (procedure?) (list?) any)]
                       [rename map list/map
                               (->* (procedure?) (list?) any)]
                       [rename append list/append
                               (->* () (list? list?) list?)]))

define (list)
  (match
    ([0 null]
     [(app pred n) cons under (list n)]))

define (fold f)
  (match
   ([(list)]
    [(cons x xs) fold (f) xs f x]))

define (map f)
  (fold (cons f) swap racket:empty)

define (append)
  (arity 2 (racket:append))
