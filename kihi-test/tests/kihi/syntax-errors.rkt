#lang racket/base

;; Checks that malformed binding forms are rejected at expansion time.  The
;; source is read from a string so that square brackets keep their shape.

(require rackunit)

(define (expand-kihi source)
  (parameterize ([current-namespace (make-base-namespace)]
                 [read-accept-reader #t])
    (expand (read-syntax 'test (open-input-string
                                (string-append "#lang kihi\n" source))))))

(define-syntax-rule (check-rejected source message)
  (check-exn (λ (e)
               (and (exn:fail:syntax? e)
                    (regexp-match? (regexp-quote message) (exn-message e))))
             (λ () (expand-kihi source))
             source))

(test-case "procedure bodies"
  (check-rejected "let (f x) [add1 x] f 1"
                  "let: expected a parenthesized procedure body")
  (check-rejected "apply λ x [add1 x] 1"
                  "λ: expected a parenthesized procedure body")
  (check-rejected "define (f x) [add1 x]"
                  "define: expected a parenthesized procedure body")
  (check-rejected "let (f x) add1 f 1"
                  "let: expected a parenthesized procedure body")
  (check-rejected "let (f x)"
                  "let: expected a procedure body"))

(test-case "binders"
  (check-rejected "apply λ (x y) (x) 1 2"
                  "λ: expected a name, a name in parentheses, or names in square brackets")
  (check-rejected "apply λ [x [y]] (x) 1 2"
                  "λ: expected a name or a name in parentheses")
  (check-rejected "define [f x] (x)"
                  "define: expected a name or a parenthesized procedure signature"))
