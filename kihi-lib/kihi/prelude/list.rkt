#lang kihi/base

require (prefix-in (racket:
                    racket)
         kihi/prelude/primitive
         kihi/prelude/procedure
         kihi/prelude/stack
         kihi/prelude/number
         racket/contract)

provide (list?
         list
         cons
         rename-out ([racket:empty nil]
                     [racket:make-list repeat]
                     [fold list/fold]
                     [map list/map]
                     [append list/append]))

define (list)
  (match
    ([0 racket:empty]
     [(app pred n) cons under (list n)]))

define (fold (f))
  (match
   ([(list)]
    [(cons x xs) fold (f) xs f x]))

define (map (f))
  (fold (cons f) swap racket:empty)

define (append)
  (with-arity (racket:append) 2)
