#lang kihi

require ("../unit.rkt")

test-case "none?"
  (check-true (none? none) "none?")

test-case "some?"
  (check-true (some? some #f) "some?")

test-case "option?"
  (check-true (option? none) "option?")

test-case "option/fold"
  (check-true (option/fold (#f) none #t) "fold none"
   check-true (option/fold () some #t) "fold some")
