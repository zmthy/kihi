#lang kihi/base

provide (string?
         rename-out ([string-length string/length])
         string/append)

define (string/append)
  (arity 2 (string-append))
