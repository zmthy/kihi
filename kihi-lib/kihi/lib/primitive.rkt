#lang racket

(require (prefix-in racket/ racket)
         "../private/program.rkt")

(provide (contract-out [apply (-> program? any)]
                       [before (-> program? any/c program?)]
                       [after (-> program? any/c program?)])
         copy
         drop)

(define (apply f)
  (racket/apply values (program->list f)))

(define (before f x)
  (program apply f x))

(define (after f x)
  (list->program (cons x (program->list f))))

(define (copy x)
  (values x x))

(define (drop x)
  (void))
