#lang kihi/base

require (racket/contract)

provide (apply
         left
         right
         copy
         drop)

define (apply (f))
  (f)

define (left (f) x)
  ((x f))

define (right (f) x)
  ((f x))

define (copy x)
  (x x)

define (drop x)
  ()
