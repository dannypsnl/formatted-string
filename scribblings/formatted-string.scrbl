#lang scribble/manual
@require[@for-label[racket/base]]

@title{formatted-string}
@author{林子篆}

@defmodulelang[formatted-string]{
The @racketmodname[formatted-string] language is a lang-extension extends racket string to formatted string.

For example, @racket[@#,hash-lang[] @#,racketmodname[formatted-string] @#,racketmodname[racket/base]] is a language like @racketmodname[racket/base], except that for example @racket["$(+ 1 2 3)"] produces @racket["6"].

@racketmod[formatted-string @#,racketmodname[racket/base]
(define-values (x y z) (values 1 2 3))
"x = $x, y = $y, z = $z, (+ x y z) = $(+ x y z)"
]
produces @racket["x = 1, y = 2, z = 3, (+ x y z) = 6"]

However, sometimes we would like to write @racket["$"] in string, in this case, you write @racket["$$"] to escape from formatted string!
}
