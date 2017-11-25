#lang racket

(require racket/base
         kihi/private/syntax
         kihi/syntax)

(provide kihi
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         (except-out (all-from-out racket/base)
                     #%module-begin
                     #%top-interaction))

(module reader syntax/module-reader kihi/base)
