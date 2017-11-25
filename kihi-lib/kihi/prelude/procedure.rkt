#lang kihi/base

require (kihi/prelude/primitive
         kihi/prelude/stack
         racket/contract)

provide (procedure?
         contract-out ([compose (-> procedure? procedure? procedure?)]
                       [flip (-> procedure? procedure?)]
                       [join (-> procedure? procedure?)]
                       [rec (-> procedure? any)]))

define (compose f g)
  ((f g))

define (flip f)
  ((f swap))

define (join f)
  ((f copy))

define (rec f)
  (f (rec (f)))
