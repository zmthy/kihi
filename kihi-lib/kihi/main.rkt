#lang racket/base

(require kihi/private/syntax
         (except-in kihi/base
                    apply compose
                    if and or
                    = < > <= >=
                    + - * /
                    list cons)
         kihi/prelude)

(provide kihi
         (all-from-out kihi/base)
         (all-from-out kihi/prelude))

(module reader syntax/module-reader kihi)
