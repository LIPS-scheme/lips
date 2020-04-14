(define-macro (t.is a b)
  "(t.is a b)

   Helper comparator for ava."
  `(--> t (is (equal? ,a ,b) #t)))

(define-macro (to.throw . body)
  "(to.throw code)

   If code throw exception it will return true, otherwise
   it will return false."
  `(try (begin ,@body #f) (catch (e) #t)))

(define (test_ . rest)
  "(test_ . rest)

   Helper that disable tests. Function that do nothing"
  )
