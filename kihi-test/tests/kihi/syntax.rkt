#lang kihi

require (prefix-in (racket: racket/base)
         "unit.rkt")

test-case "racket"
  (check-true (racket (racket:if #t #t #f)) "racket")

racket/stmt (define x #t)

test-case "racket/stmt"
  (check-true (x) "racket/stmt")

test-case "define"
  (define (y) (#t)
   check-true (y) "define")

test-case "struct"
  (struct (y x)
   check-true (y-x y #t) "struct")

test-case "bind"
  (check-true (bind (y) (not y) #f) "bind")

test-case "let"
  (check-true (let ([(y) #t]) (y)) "let")

test-case "λ"
  (check-true (apply λ (y) (y) #t) "lambda")

test-case "match"
  (check-true (match ([1 #f] [2 #t]) 2) "match")

test-case "arity"
  (check-equal? (arity 2 (racket:+) 1 2) (3) "arity")
