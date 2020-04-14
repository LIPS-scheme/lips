(define-macro (t.is a b)
  "(t.is a b)

   Helper comparator for ava."
  (let ((result (gensym)))
    `(let ((,result (--> t (is (equal? ,a ,b) #t))))
       (if (not ,result)
           (throw (new Error (concat "failed: " (repr ',a) " is not equal " (repr ',b)))))
       ,result)))

(define-macro (to.throw . body)
  "(to.throw code)

   If code throw exception it will return true, otherwise
   it will return false."
  (let ((result (gensym)))
    `(let ((,result (try (begin ,@body #f) (catch (e) #t))))
       (if (not ,result)
           (throw (new Error (concat "failed: " ',body))))
       ,result)))

(define (test_ . rest)
  "(test_ . rest)

   Helper that disable tests. Function that do nothing"
  )
