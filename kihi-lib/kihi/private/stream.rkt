#lang racket

(provide stream)

(define (stream form)
  (match form
    [(list) (void)]
    [(cons term stack)
     (stream (cond
               [(procedure? term) (execute term stack)]
               [else (println term)
                     stack]))]))


(define (execute term stack)
  (if (procedure? term)
      (let ([arity (match (procedure-arity term)
                     [(? number? arity) arity]
                     [(arity-at-least arity) arity]
                     [(cons arity _) arity])])
        (let-values ([(args stack) (collect arity stack)])
          (call-with-values
           (λ () (apply term args))
           (λ result
             (append (filter (not/c void?) result) stack)))))
      (match stack
        [(list) (list term)]
        [(cons next stack) (cons term (execute next stack))])))


(define (collect arity stack)
  (let ([args (take stack arity)])
    (if (andmap (not/c procedure?) args)
        (split-at stack arity)
        (collect arity (match stack
                         [(list) (error "not enough values")]
                         [(cons term stack)
                          (execute term stack)])))))
