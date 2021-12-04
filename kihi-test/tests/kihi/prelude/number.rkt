#lang kihi

require ("../unit.rkt")

test-case "number?"
  (check-true (number? 5) "number?")

test-case "add1"
  (check-equal? (add1 4) (5) "add1")

test-case "sub1"
  (check-equal? (sub1 4) (3) "sub1")

test-case "="
  (check-true (= 2 2) "=")

test-case "<"
  (check-true (< 2 3) "<")

test-case ">"
  (check-true (> 3 2) ">")

test-case "<="
  (check-true (<= 2 3) "<=")

test-case ">="
  (check-true (>= 3 2) ">=")

test-case "+"
  (check-equal? (+ 6 3) (9) "+")

test-case "-"
  (check-equal? (- 6 3) (3) "-")

test-case "*"
  (check-equal? (* 6 3) (18) "*")

test-case "/"
  (check-equal? (/ 6 3) (2) "/")
