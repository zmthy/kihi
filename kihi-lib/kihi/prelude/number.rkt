#lang kihi/base

require (prefix-in (racket:
                    racket/base)
         only-in (racket/match
                  define-match-expander)
         kihi/prelude/procedure
         kihi/prelude/stack)

provide (number?
         rename-out ([add1 suc]
                     [sub1 pred])
         + - * /)

define (+)
  (with-arity (racket:+) 2)

define (-)
  (with-arity (racket:-) 2)

define (*)
  (with-arity (racket:*) 2)

define (/)
  (with-arity (racket:/) 2)
