#lang kihi/base

require (racket/contract)

provide (contract-out ([apply (-> procedure? any)]
                       [left (-> procedure? any/c procedure?)]
                       [right (-> procedure? any/c procedure?)])
         copy
         drop)

define (apply f)
  (f)

define (left f x)
  ((x f))

define (right f x)
  ((f x))

define (copy x)
  (x x)

define (drop x)
  ()
