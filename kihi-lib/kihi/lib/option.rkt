#lang racket

(require "primitive.rkt"
         "program.rkt")

(provide none
         some
         none?
         some?
         option?
         (contract-out [option/fold (-> program? option? any)]))

(define-struct none ())

(define-struct some (value))

(define option? (or/c none? some?))

(define (option/fold f x)
  (match x
    [(some value) (values apply f value)]
    [(none) (values)]))

(define (option/map f x)
  (option/fold (program some apply f) x))
