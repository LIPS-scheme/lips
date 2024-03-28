(set-special! "#:" 'frob lips.specials.LITERAL)

(define (frob filename)
  (call-with-input-file filename
    (lambda (port)
      (read port))))

#:"data.scm"

(print x)
