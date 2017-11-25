#lang kihi

require ("../unit.rkt")

test-case "boolean?"
  (check-true (boolean? #f) "boolean?")

test-case "not"
  (check-true (not #f) "not")

test-case "if"
  (check-true (if #f (#f) (#t)) "if")

test-case "and"
  (check-true (and #t (#t)) "and")

test-case "or"
  (check-true (or #f (#t)) "or")
