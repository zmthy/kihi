#lang kihi/base

require (kihi/prelude/primitive
         kihi/prelude/stack
         racket/contract)

provide (pair?
         rename-out ([cons pair])
         contract-out ([unpair (-> pair? any)]
                       [rename car first
                               (-> pair? any)]
                       [rename cdr second
                               (-> pair? any)]
                       [rename fold pair/fold
                               (->* (procedure?) (pair?) any)]
                       [rename map pair/map
                               (->* (procedure? procedure?) (pair?) any)]))

define (unpair)
  (car under (cdr) copy)

define (fold f)
  (f unpair)

define (map f g)
  (fold (f under (g)))
