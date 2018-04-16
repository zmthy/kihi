#lang kihi/base

require (kihi/prelude/primitive
         kihi/prelude/stack
         racket/contract)

provide (procedure?
         compose
         flip
         join
         rec
         with-arity)

define (compose (f) (g))
  ((f g))

define (flip (f))
  ((f swap))

define (join (f))
  ((f copy))

define (rec (f))
  (f (rec (f)))

define (with-arity f n)
  (apply procedure-reduce-arity f n)
