(define-macro (t.is a b)
  "(t.is a b)

   Helper comparator for ava. It use equal? so it match two lists and strings.
   It use undecumented API that allow to delete StackTrace when assertion fail."
  (let ((attempt (gensym))
        (a_name (gensym))
        (b_name (gensym)))
    `(let ((,attempt (t.try (lambda (e)
                              (let ((,a_name ,a)
                                    (,b_name ,b))
                                (if (equal? ,a_name ,b_name)
                                    (--> e (pass))
                                    (--> e (fail (concat "failed: " (repr ,a_name true)
                                                         " != " (repr ,b_name true)
                                                         " in " (repr ',a true))))))))))
       (if (not (. ,attempt 'passed))
           (--> (. ,attempt 'errors)
                (forEach (lambda (e)
                           (set-obj! e 'savedError #void)))))
       (--> ,attempt (commit)))))


(define-macro (to.throw . body)
  "(to.throw code)

   If code throw exception it will return true, otherwise
   it will return false."
  (let ((result (gensym)))
    `(try (begin ,@body #f) (catch (e) #t))))

(define (%test-specs t specs)
  "(%test-specs t list)

   Function test list of specs (\"name\" function result . args)"
  (let iter ((specs specs))
    (if (not (null? specs))
        (let* ((spec (car specs))
               (name (car spec))
               (fn (cadr spec))
               (expected (caddr spec))
               (args (cdddr spec)))
          (let ((result (apply fn args)))
            (if (not (equal? result expected))
                (error (string-append "FAILED: "
                                      (repr (apply list name expected args) true))))
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
