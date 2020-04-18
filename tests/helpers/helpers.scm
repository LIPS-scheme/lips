(define-macro (t.is a b)
  "(t.is a b)

   Helper comparator for ava. It use equal? so it match two lists and strings."
  (let ((result (gensym)))
    `(let ((,result (--> t (is (equal? ,a ,b) #t))))
       ;;(if (not ,result)
       ;;    (throw (new Error (concat "failed: " (repr ',a) " is not equal " (repr ',b)))))
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


;;(define t.is equal?)

(define (%test-specs t specs)
  "(%test-specs t list)

   Funtion test list of specs (\"name\" function result . args)"
  (let iter ((specs specs))
    (if (not (null? specs))
        (let* ((spec (car specs))
               (name (car spec))
               (fn (cadr spec))
               (expected (caddr spec))
               (args (cdddr spec)))
          (let ((result (apply fn args)))
            (if (not (equal? result expected))
                (error (string-append "FAILED: " (repr (apply list name expected args) true))))
            (t.is result expected))
          (iter (cdr specs))))))

(define-macro (test-specs . body)
  "(test-specs . body)

   Macro that simplify calling %test-specs, Body need to be list
   of function calls where second argument is result the function.
   e.g.:

      (string=? #t \"foo\" \"bar\")  ;; this will fail"
  `(%test-specs t (list ,@(map (lambda (spec)
                                 `(list ,(symbol->string (car spec))
                                        ,@spec))
                               body))))
