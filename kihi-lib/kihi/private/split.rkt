#lang racket

(provide splitf-at)

(define (splitf-at lst pred pos)
  (split empty lst pred pos))

(define (split fst lst pred pos)
  (if (or (zero? pos)
          (empty? lst)
          (not (pred (first lst))))
      (values (reverse fst) lst pos)
      (split (cons (first lst) fst) (rest lst) pred (sub1 pos))))
