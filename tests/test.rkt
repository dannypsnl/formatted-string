#lang formatted-string racket

(require rackunit)

(define-values (x y z) (values 1 2 3))

(check-equal? "x = $x, y = $y, z = $z, (+ x y z) = $(+ x y z)"
              "x = 1, y = 2, z = 3, (+ x y z) = 6")
(check-equal? "(+ 1 2) = $(+ 1 2)\$"
              "(+ 1 2) = 3\$")
