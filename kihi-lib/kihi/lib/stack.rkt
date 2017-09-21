#lang racket

(require "primitive.rkt"
         "../private/program.rkt")

(provide swap
         (contract-out [apply-with (-> any/c program? any)]
                       [under (-> program? any/c any)]
                       [under₂ (-> program? any/c any/c any)]
                       [over (-> program? any/c any)])
         swap-over
         swap-under
         copy-over
         copy-under)

(define (swap x y)
  (values y x))

(define (apply-with x f)
  (values apply f x))

(define (under f x)
  (values x apply f))

(define (under₂ f x y)
  (values x y apply f))

(define (over f x)
  (values swap under f))

(define (swap-over x y z)
  (values z x y))

(define (swap-under x y z)
  (values y z x))

(define (copy-over x y)
  (values y x y))

(define (copy-under x y)
  (values x y x))
