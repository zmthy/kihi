#lang racket

(require racket/struct)

(provide run-values
         run-print
         run-stream
         program
         execute
         execute-if-procedure
         no-execute)

(define (run-values . forms)
  (match (stream->list (apply run-stream forms))
    ;; Rather than return no values, return void instead.
    [(list) (void)]
    [result (apply values result)]))

(define (run-print . forms)
  (stream-for-each println (apply run-stream forms)))

(define (run-stream . forms)
  (run-forms (in-list (map execute-if-procedure forms))))

(define-syntax-rule (program forms ...)
  (procedure-rename (λ args (apply run-values forms ... args))
                    'program))

(struct execute (procedure)
  #:property prop:object-name (const 'curried)
  #:property prop:procedure (struct-field-index procedure)
  #:methods gen:custom-write
  [(define (write-proc e port mode)
     (display "#<procedure:curried>" port))])

(struct no-execute (value)
  #:property prop:object-name (const #f)
  #:property prop:procedure (struct-field-index value)
  #:methods gen:custom-write
  [(define (write-proc e port mode)
     (display "#<procedure>" port))])

(define wrap-with-execute?
  (conjoin procedure? (negate execute?)))

(define (execute-if-procedure value)
  (match value
    [(no-execute value) value]
    [(? wrap-with-execute?) (execute value)]
    [_ value]))

(define (run-forms forms)
  (if (stream-empty? forms)
      forms
      (match/values (unstream forms)
        ;; Apply a procedure when it is wrapped in an execute structure.
        [((execute proc) forms)
         (run-procedure proc forms)]
        ;; Otherwise just stream out the value.
        [(value forms)
         (stream-cons value (run-forms forms))])))

(define (run-procedure proc forms)
  (match (procedure-simple-arity proc)
    [0 (run-forms (append-results proc forms))]
    [arity
     (match/values (stream-split-at (run-forms forms) arity)
       ;; Another procedure has gotten stuck before all of our arguments have
       ;; been made available, compose the two together.
       [((and args (cons (execute stuck) _)) _ _)
        (stuck-program proc args (procedure-simple-arity stuck))]
       ;; We have all of our arguments, apply the procedure.
       [(args forms 0)
        (run-forms (append-results (thunk (apply proc (reverse args))) forms))]
       ;; We do not have all of our arguments, curry the procedure.
       [(args _ n)
        (stuck-program proc args n)])]))

(define (stuck-program proc args arity)
  (stream
   (execute
    (procedure-reduce-arity
     (λ vals
       (apply values
              (stream->list
               (run-forms (in-list (cons (execute proc)
                                         (append (reverse args) vals)))))))
     (arity-at-least arity)))))

(define (procedure-simple-arity proc)
  (match (procedure-arity proc)
    [(? number? arity) arity]
    [(arity-at-least arity) arity]
    [(cons arity _) arity]
    [(list) 0]))

(define (append-results proc forms)
  (call-with-values
   proc
   (λ results (stream-append (in-list (filter (negate void?) results)) forms))))

(define (stream-split-into out in n)
  (if (or (zero? n) (stream-empty? in))
      (values out in n)
      (stream-split-into
       (cons (stream-first in) out)
       (stream-rest in)
       (sub1 n))))

(define stream-split-at
  (curry stream-split-into empty))

(define (unstream s)
  (values (stream-first s) (stream-rest s)))
