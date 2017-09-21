#lang racket

(provide (rename-out [make program]
                     [program-body program->list])
         list->program
         program?
         program-body)

(struct program (body)
  #:constructor-name list->program)

(define (make . body)
  (list->program body))
