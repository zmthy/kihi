#lang kihi

require ("../unit.rkt")

test-case "pair?"
  (check-true (pair? pair 1 2) "pair?")

test-case "unpair"
  (check-true (drop unpair pair #f #t) "unpair")

test-case "first"
  (check-true (first pair #t #f) "first")

test-case "second"
  (check-true (second pair #f #t) "second")

test-case "pair/fold"
  (check-true (pair/fold (drop) pair #f #t) "pair/fold")

test-case "pair/map"
  (check-true (drop pair/map (not) (not) pair #t #f) "pair/map")
