#lang kihi/base

require (prefix-in (racket:
                    racket/base)
         racket/contract)

provide (boolean?
         if
         and
         or)

define (if x (f) (g))
  (racket (racket:if (kihi x) (kihi f) (kihi g)))

define (and x (f))
  (racket (racket:and (kihi x) (kihi f)))

define (or x (f))
  (racket (racket:or (kihi x) (kihi f)))
