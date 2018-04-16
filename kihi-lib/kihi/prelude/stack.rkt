#lang kihi/base

require (kihi/prelude/primitive
         racket/contract)

provide (apply-with
         under
         under₂
         over
         split
         swap
         swap₂
         swap-over
         swap-under
         copy-over
         copy-under)

define (swap x y)
  (y x)

define (swap₂ x y z)
  (z y x)

define (apply-with x (f))
  (f x)

define (under (f) x)
  (x f)

define (under₂ (f) x y)
  (x y f)

define (over (f) x)
  (swap x f)

define (swap-over x y z)
  (z x y)

define (swap-under x y z)
  (y z x)

define (copy-over x y)
  (y x y)

define (copy-under x y)
  (x y x)

define (split (f) (g) x)
  (f x g x)
