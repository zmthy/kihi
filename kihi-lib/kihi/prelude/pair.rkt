#lang kihi/base

require (kihi/prelude/primitive
         kihi/prelude/stack
         racket/contract)

provide (pair?
         unpair
         rename-out ([cons pair]
                     [car first]
                     [cdr second]
                     [fold pair/fold]
                     [map pair/map]))

define (unpair)
  (car under (cdr) copy)

define (fold (f))
  (f unpair)

define (map (f) (g))
  (fold (f under (g)))
