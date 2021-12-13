(module reader syntax/module-reader
  #:language read
  #:wrapper2
  (λ (in rd stx?)
    (parameterize ([current-readtable (make-readtable #f #\" 'terminating-macro action)])
      (define stx (rd in))
      (define old-prop (syntax-property stx 'module-language))
      (define new-prop `#(formatted-string/lang/language-info get-language-info ,old-prop))
      (syntax-case (syntax-property stx 'module-language new-prop) ()
        [(module name lang (#%module-begin . body))
         (let ((make-unhygienic (make-syntax-delta-introducer #'require #'module)))
           (wrap
            #`(module name lang
                (#%module-begin
                 . body))))])))

  (define wrap (make-syntax-introducer #t))
  (define (action c in src line col pos)
    (define (conv src in)
      (define args '())
      (define s
        (let loop ([l '()])
          (define c (peek-char in))
          (cond
            [(eof-object? c) l]
            [(char=? c #\") (read-char in) l]
            [(char=? c #\$) (read-char in)
                            (set! args (append args (list (read-syntax src in))))
                            (loop (append l (list #\~ #\a)))]
            [(char=? c #\\) (read-char in)
                            (cond
                              [(char=? (peek-char in) #\$)
                               (loop (append l (list (read-char in))))]
                              [else (loop (append l (list #\\ c)))])]
            [else (read-char in)
                  (loop (append l (list c)))])))
      (if (= (length args) 0)
          (list->string s)
          (with-syntax ([fmt (list->string s)]
                        [(arg ...) args])
            (syntax/loc (srcloc src line col pos #f)
              (#,(wrap #'format) fmt arg ...)))))
    (conv src in)))
