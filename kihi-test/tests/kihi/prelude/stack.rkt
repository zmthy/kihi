#lang kihi

require ("../unit.rkt")

test-case "swap"
  (check-true (drop swap #t #f) "swap then drop")

test-case "swap₂"
  (check-true (drop drop swap₂ #t #f #f) "swap₂ then drop drop")

test-case "apply-with"
  (check-true (apply-with #t ()) "apply-with identity")

test-case "under"
  (check-true (drop under (#t) #f) "true under false")

test-case "under₂"
  (check-true (drop drop under₂ (#t) #f #f) "true under₂ false")

test-case "over"
  (check-true (drop over (#f) #t) "false over true")

test-case "swap-over"
  (check-equal? (list 3 swap-over 2 3 1)
                (list 3 1 2 3)
                "swap-over")

test-case "swap-under"
  (check-equal? (list 3 swap-under 3 1 2)
                (list 3 1 2 3)
                "swap-under")

test-case "copy-over"
  (check-equal? (list 3 copy-over 1 2)
                (list 3 2 1 2)
                "copy-over")

test-case "copy-under"
  (check-equal? (list 3 copy-under 1 2)
                (list 3 1 2 1)
                "copy-under")

test-case "split"
  (check-equal? (pair split () (not) #t)
                (pair #t #f)
                "left drop")
