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

define either (or/c)
define is-none (none?)
define is-some (some?)

define (option?)
  (apply with-arity either 2 is-none is-some)

define (fold (f))
  (match
    ([(some v) f v]
     [(none)]))

define (map (f))
  (fold (some f))
