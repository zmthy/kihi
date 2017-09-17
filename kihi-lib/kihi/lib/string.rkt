#lang racket

(provide string?
         (rename-out [string-length string/length])
         (contract-out [string/append (-> string? string? string?)]))

(define (string/append x y)
  (string-append x y))
