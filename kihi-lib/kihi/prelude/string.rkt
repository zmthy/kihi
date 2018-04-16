#lang kihi/base

require (kihi/prelude/procedure)

provide (string?
         rename-out ([string-length string/length])
         string/append)

define (string/append)
  (with-arity (string-append) 2)
