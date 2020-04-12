(define-macro (t.is a b)
  `(--> t (is (equal? ,a ,b) true)))
