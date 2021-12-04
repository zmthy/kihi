#lang kihi

require (racket/function
         except-in (rackunit
                    test-case
                    check-true
                    check-equal?)
         prefix-in [racket: racket]
         prefix-in [rackunit: rackunit])

provide (test-case
         check-true
         check-equal?)

define (count-results)
  (= length)

define (test (f) (next) msg)
  (racket
   (call-with-values
    (thunk (kihi f))
    (λ results
      (let ([results (filter (negate void?) results)])
        (test-begin
          (kihi with-arity (check) 4
                (count-results) results 1
                string/append "result count: " msg)
          (kihi next first results msg))))))

define (test-case name (body))
  (racket (rackunit:test-case (kihi name) (kihi body)))

define (check-true f)
  (test f (with-arity (rackunit:check-true) 2))

define (check-equal? f g)
  (test f (test g right (with-arity (rackunit:check-equal?) 3)))
