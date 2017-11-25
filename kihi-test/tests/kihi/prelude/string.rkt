#lang kihi

require ("../unit.rkt")

test-case "string?"
  (check-true (string? "") "string?")

test-case "string/length"
  (check-equal? (string/length "abcde") (5) "string/length")

test-case "string/append"
  (check-equal? (string/append "ab" "cd") ("abcd") "string/append")
