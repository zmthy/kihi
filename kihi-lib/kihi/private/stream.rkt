#lang racket

(require "split.rkt")

(provide stream)

(define (stream . forms)
  (stream-loop empty forms))

(define (stream-loop results forms)
  (match forms
    ;; If there are no forms left, return the results in the order they arrived.
    [(list)
     (apply values (reverse results))]
    ;; If there is a term on the stack to process, then either apply it or add
    ;; it to the list of results.
    [(cons term stack)
     (cond
       [(procedure? term)
        (stream-loop results (execute term stack))]
       [else
        (stream-loop (cons term results) stack)])]
    ;; If the tail of the forms list turns out to be a procedure, the stack ran
    ;; out of values before if could finish evaluation.  The resulting curried
    ;; procedure represents the rest of the computation once more values become
    ;; available.  In this case, the procedure is the last result of the stream.
    [(? procedure?)
     (stream-loop (cons forms results) empty)]))

(define (execute term stack)
  (let ([arity (match (procedure-arity term)
                 [(? number? arity) arity]
                 [(arity-at-least arity) arity]
                 [(cons arity _) arity])])
    (let*-values ([(args stack remaining) (collect arity stack)])
      (if (zero? remaining)
          (call-with-values
           (λ () (apply term args))
           (λ result
             (append (filter (not/c void?) result) stack)))
          ;; There were not enough values left on the stack to apply the
          ;; procedure.  Return a curried form instead.
          (procedure-reduce-arity (apply curry term args) remaining)))))

(define (collect arity stack)
  (if (procedure? stack)
    (values stack empty)
    (let*-values ([(vals leftover remaining)
                   (splitf-at stack (negate procedure?) arity)])
      (match leftover
        ;; We have collected enough values, or there is nothing left.
        [(or (? (const (zero? remaining)))
             (list))
         (values vals leftover remaining)]
        ;; There's a procedure in the way, so execute it.
        [(cons proc stack)
         (let-values ([(args stack remaining)
                       (collect remaining (execute proc stack))])
           (values (append vals args) stack remaining))]))))
