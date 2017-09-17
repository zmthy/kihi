#lang racket

(require (prefix-in racket/ racket)
         "primitive.rkt"
         "program.rkt")

(provide boolean?
         (contract-out [if (-> boolean? program? program? any)]
                       [and (-> boolean? boolean? boolean?)]
                       [or (-> boolean? boolean? boolean?)]))

(define (if x f g)
  (values apply (racket/if x f g)))

(define (and x y)
  (racket/and x y))

(define (or x y)
  (racket/or x y))
