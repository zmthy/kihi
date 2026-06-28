#lang kihi

require (prefix-in (racket: racket/base)
         "unit.rkt")

test-case "racket"
  (check-true (racket (racket:if #t #t #f)) "racket"
   check-true (drop racket (values #f #t)) "multiple values")

racket/stmt (define x #t)

test-case "racket/stmt"
  (check-true (x) "racket/stmt")

test-case "define"
  (define (y) (#t)
   check-true (y) "procedure"
   define z #t
   check-true (z) "value"
   define w (5)
   check-equal? (apply w) (5) "procedure value")

test-case "struct"
  (struct (y x)
   check-true (y-x y #t) "struct")

test-case "let"
  (check-true (let y #t y) "basic"
   check-equal? (let fact λ (n) (apply (match ([0 1] [m * m apply fact - m 1])) n) apply fact 5) (120) "self-reference"
   check-true (let x #f let y not x y) "nested"
   check-true (let x #f let x #t x) "shadowing"
   check-equal? (let (x y) 1 2 + x y) (3) "multi-variable"
   check-equal? (let (x y) 1 add1 x y) (2) "let* style")

test-case "λ"
  (check-true (apply λ (y) (y) #t) "lambda"
   check-true (apply λ y (y) #t) "bare binding")

test-case "match"
  (check-true (match ([1 #f] [2 #t]) 2) "match")
