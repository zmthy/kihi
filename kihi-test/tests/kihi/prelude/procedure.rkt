#lang kihi

require (prefix-in (racket: racket)
         "../unit.rkt")

test-case "compose"
  (check-equal? (apply compose (pair 1) (pair 2) nil)
                (list 2 1 2)
                "compose")

test-case "flip"
  (check-equal? (pair apply flip (+ 5) 1 2)
                (pair 7 1)
                "flip")

test-case "join"
  (check-equal? (pair apply join () #t)
                (pair #t #t)
                "join")

test-case "rec"
  (check-true (rec (drop) #t)
              "rec")

test-case "with-arity"
(check-equal? (with-arity (racket:+) 2 1 2)
              (3)
              "with-arity")
