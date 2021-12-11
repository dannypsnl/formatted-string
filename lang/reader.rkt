(module reader racket/base
  (require syntax/module-reader)

  (provide (rename-out [formatted-string-read read]
                       [formatted-string-read-syntax read-syntax]
                       [formatted-string-get-info get-info]))

  (define-values (formatted-string-read formatted-string-read-syntax formatted-string-get-info)
    (make-meta-reader
     'formatted-string
     "language path"
     (lambda (bstr)
       (let* ([str (bytes->string/latin-1 bstr)]
              [sym (string->symbol str)])
         (and (module-path? sym)
              (vector
               ;; try submod first:
               `(submod ,sym reader)
               ;; fall back to /lang/reader:
               (string->symbol (string-append str "/lang/reader"))))))
     (lambda (orig-read)
       (define (read . args)
         (apply orig-read args))
       read)
     (lambda (orig-read-syntax)
       (define (read-syntax . args)
         (define stx (apply literal-read-syntax args))
         (define old-prop (syntax-property stx 'module-language))
         (define new-prop `#(formatted-string/lang/language-info get-language-info ,old-prop))
         (syntax-property stx 'module-language new-prop))
       read-syntax)
     (lambda (proc) proc)))

  (require syntax/strip-context
           racket/syntax-srcloc)
  (define (literal-read-syntax src in s a b c)
    (define ss
      (let loop ([r '()])
        (define stx (read-syntax src in))
        (if (eof-object? stx)
            r
            (loop (append r (list stx))))))
    (with-syntax ([(s ...) (map (lambda (stx)
                                  ; walk on stx
                                  (if (string? (syntax->datum stx))
                                      (embed-computation-into-string src stx (syntax->datum stx))
                                      stx))
                                ss)])
      (strip-context
       #'(module anything racket/base
           s ...))))

  (define (embed-computation-into-string src origin-stx S)
    (define idx (string-index S "$"))
    (if idx
        (let* ([exp (read-syntax src (open-input-string (substring S (add1 idx))))]
               [end-idx (+ idx (string-length (format "~a" (syntax->datum exp))))]
               [after-stx (embed-computation-into-string src origin-stx (substring S (add1 end-idx)))])
          (with-syntax ([fmt (string-append (substring S 0 idx) "~a")]
                        [e exp]
                        [a-stx after-stx])
            (syntax/loc
                (syntax-srcloc origin-stx)
              (format (string-append fmt a-stx) e))))
        (syntax/loc (syntax-srcloc origin-stx) "")))

  (define (string-index hay needle)
    (define n (string-length needle))
    (define h (string-length hay))
    (and (<= n h) ; if the needle is longer than hay, then the needle can not be found
         (for/or ([i (- h n -1)]
                  #:when (string=? (substring hay i (+ i n)) needle))
           i))))
