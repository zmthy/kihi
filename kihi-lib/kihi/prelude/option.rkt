#lang kihi/base

require (only-in (racket/contract
                  or/c)
         kihi/prelude/primitive
         kihi/prelude/procedure
         kihi/private/runtime
         racket/contract)

provide (none
         some
         none?
         some?
         option?
         rename-out ([fold option/fold]
                     [map option/map]))

struct (none)

struct (some value)

define (option?)
  (apply with-arity (or/c) 2 (none?) (some?))

define (fold (f))
  (match
    ([(some v) f v]
     [(none)]))

define (map (f))
  (fold (some f))
