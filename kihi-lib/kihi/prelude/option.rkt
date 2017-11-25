#lang kihi/base

require (only-in (racket/contract
                  or/c)
         kihi/private/runtime
         kihi/prelude/primitive
         racket/contract)

provide (none
         some
         none?
         some?
         option?
         contract-out ([rename fold option/fold
                               (->* (procedure?) (option?) any)]
                       [rename map option/map
                               (->* (procedure?) (option?) any)]))

struct (none)

struct (some value)

define (option?)
  (apply arity 2 (or/c) (none?) (some?))

define (fold f)
  (match
    ([(some v) f v]
     [(none)]))

define (map f)
  (fold (some f))
