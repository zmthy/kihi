#lang racket

(provide split-at
         splitf-at)

(define (split-at lst pos)
  (splitf-at lst (const #t) pos))

(define (splitf-at lst pred pos)
  (split empty lst pred pos))

(define (split fst lst pred pos)
  (if (or (zero? pos)
          (empty? lst)
          (not (pred (first lst))))
      (values (reverse fst) lst pos)
      (split (cons (first lst) fst) (rest lst) pred (sub1 pos))))
