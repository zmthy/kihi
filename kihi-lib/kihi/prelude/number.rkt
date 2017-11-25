#lang kihi/base

require (prefix-in (racket:
                    racket/base)
         only-in (racket/match
                  define-match-expander)
         kihi/prelude/stack)

provide (number?
         rename-out ([add1 suc]
                     [sub1 pred])
         + - * /)

define (+)
  (arity 2 (racket:+))

define (-)
  (arity 2 (racket:-))

define (*)
  (arity 2 (racket:*))

define (/)
  (arity 2 (racket:/))
