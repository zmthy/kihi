#lang racket

(require (prefix-in racket/ racket))

(provide number?
         (contract-out (rename add1 suc (-> number? number?))
                       [+ (-> number? number? number?)]
                       [- (-> number? number? number?)]
                       [* (-> number? number? number?)]
                       [/ (-> number? number? number?)]))

(define (+ x y)
  (racket/+ x y))

(define (- x y)
  (racket/- x y))

(define (* x y)
  (racket/* x y))

(define (/ x y)
  (racket// x y))
