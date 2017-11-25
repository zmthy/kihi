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

define (count-results results expected)
  (= length results expected)

define (test f next msg)
  (racket (call-with-values
           (thunk (kihi f))
           (λ results
             (let ([results (filter (negate void?) results)])
               (test-begin
                 (kihi arity 4 (check)
                       (count-results) results 1
                       string/append "result count: " msg)
                 (kihi next first results msg))))))

define (test-case name body)
  (racket (rackunit:test-case name body))

define (check-true f)
  (test (f) (arity 2 (rackunit:check-true)))

define (check-equal? f g)
  (test (f) (test (g) right (arity 3 (rackunit:check-equal?))))
