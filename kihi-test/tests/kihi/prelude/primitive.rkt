#lang kihi

require ("../unit.rkt")

check-true (#t) "basic"

test-case "apply"
  (check-true (apply (#t)) "apply"
   check-true (apply (apply (#t))) "nested apply")

test-case "left"
  (check-true (apply left () #t) "left"
   check-equal? (pair apply left (2) 1) (pair 1 2) "left value")

test-case "right"
  (check-true (apply right () #t) "right "
   check-equal? (pair apply right (1) 2) (pair 1 2) "right value")

test-case "copy"
  (check-equal? (pair copy #t) (pair #t #t) "basic copy")

test-case "drop"
  (check-true (drop #f #t) "basic drop")
