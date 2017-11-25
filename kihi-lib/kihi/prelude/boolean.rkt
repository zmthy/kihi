#lang kihi/base

require (prefix-in (racket:
                    racket/base)
         racket/contract)

provide (boolean?
         contract-out ([if (-> boolean? procedure? procedure? any)]
                       [and (-> boolean? procedure? any)]
                       [or (-> boolean? procedure? any)]))

define (if x f g)
  (racket (racket:if (kihi x) (kihi f) (kihi g)))

define (and x f)
  (racket (racket:and (kihi x) (kihi f)))

define (or x f)
  (racket (racket:or (kihi x) (kihi f)))
