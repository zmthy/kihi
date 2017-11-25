#lang racket

(require kihi/private/syntax)

(provide kihi)

(define-syntax (kihi stx)
  (with-syntax ([(_ forms ...) stx])
    (begin-values #'(forms ...))))

(define-syntax (kihi-stream stx)
  (with-syntax ([(_ forms ...) stx])
    (begin-stream #'(forms ...))))
