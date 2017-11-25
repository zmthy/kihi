#lang kihi

require (prefix-in (racket: racket/list)
         "../unit.rkt")

test-case "list?"
  (check-true (list? nil) "list?")

test-case "list"
  (check-true (racket:second list 3 #f #t #f) "list")

test-case "cons"
  (check-true (racket:first cons #t nil) "cons")

test-case "repeat"
  (check-equal? (length repeat 3 #t) (3) "repeat")

test-case "list/fold"
  (check-true (list/fold (or) cons #t nil (#f)) "list/fold")

test-case "list/map"
  (check-true (first list/map (not) cons #f nil) "list/map")

test-case "list/append"
  (check-equal? (length list/append cons #f nil cons #f nil) (2) "list/append")
