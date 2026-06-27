#lang kihi/base

require (prefix-in (racket:
                    racket/base)
         only-in (racket/match
                  define-match-expander)
         kihi/prelude/procedure
         kihi/prelude/stack)

provide (number?
         rename-out ([add1 suc]
                     [sub1 pred])
         = < > <= >=
         + - * /)

define equal (racket:=)
define less (racket:<)
define greater (racket:>)
define less-equal (racket:<=)
define greater-equal (racket:>=)
define plus (racket:+)
define minus (racket:-)
define times (racket:*)
define divide (racket:/)

define (=)
  (with-arity equal 2)

define (<)
  (with-arity less 2)

define (>)
  (with-arity greater 2)

define (<=)
  (with-arity less-equal 2)

define (>=)
  (with-arity greater-equal 2)

define (+)
  (with-arity plus 2)

define (-)
  (with-arity minus 2)

define (*)
  (with-arity times 2)

define (/)
  (with-arity divide 2)
