string-extension
================
The language extends racket string to formatted string, let's look at the following example.

```racket
#lang formatted-string racket/base

(define-values (x y z) (values 1 2 3))
"x = $x, y = $y, z = $z, (+ x y z) = $(+ x y z)"
```
