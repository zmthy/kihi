#lang racket

(require (prefix-in racket/ racket))

(provide (contract-out [apply (-> list? any)]
                       [before (-> list? any/c list?)]
                       [after (-> list? any/c list?)])
         copy
         drop)

(define (apply f)
  (racket/apply values f))

(define (before f x)
  (list apply f x))

(define (after f x)
  (cons x f))

(define (copy x)
  (values x x))

(define (drop x)
  (void))
