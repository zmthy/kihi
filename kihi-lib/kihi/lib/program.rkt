#lang racket

(require "../private/program.rkt"
         "primitive.rkt"
         "stack.rkt")

(provide program?
         (contract-out [compose (-> program? program? program?)]
                       [join (-> program? program?)]
                       [Y (-> program? any)]))

(define (compose f g)
  (program apply f apply g))

(define (flip f)
  (program apply f swap))

(define (join f)
  (program apply f copy))

(define (Y f)
  (values before copy before
          (program apply under
                   (program before before
                            (program apply) copy))))
