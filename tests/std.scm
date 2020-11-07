(test "std: case-lambda"
      (lambda (t)

        ;; example from R7RS without do macro
        (define range
          (case-lambda
           ((e) (range 0 e))
           ((b e) (let iter ((result ()) (i (- e 1)))
                    (if (< i b)
                        result
                        (iter (cons i result) (- i 1)))))))

        (t.is (range 3) '(0 1 2))
        (t.is (range 3 5) '(3 4))))

(test "std: string-map"
      (lambda (t)
        ;; example from R7RS spec
        (define result (string-map
                        (lambda (c k)
                          ((if (eqv? k #\u) char-upcase char-downcase)
                           c))
                        "studlycaps xxx"
                        "ululululul"))

        (t.is result "StUdLyCaPs")))

(test "std: vector-map"
      (lambda (t)

        ;; examples from R7RS spec
        (t.is (vector-map cadr '#((a b) (d e) (g h)))
              '#(b e h))

        (t.is (vector-map + '#(1 2 3) '#(4 5 6 7))
              '#(5 7 9))))

(test "std: some"
      (lambda (t)
        (t.is (some + nil) #f)
        (t.is (some odd? (list 1 2 3)) #t)
        (t.is (some odd? (list 2 4 6)) #f)))

(test "std: fold"
      (lambda (t)
        (t.is (fold * 1 (cdr (range 10))) 362880)))

(test "std: pluck"
      (lambda (t)
        (let ((name (pluck '__name__)))
          (t.is (name 'foo) "foo"))
        (let ((name (pluck "__name__")))
          (t.is (name 'foo) "foo"))
        (let ((none (pluck)))
          (t.is (none 'foo) nil))
        (let ((xy (pluck 'x 'y)))
          (t.is (xy &(:x 10 :y 20 :z 30)) &(:x 10 :y 20)))))

(test "std: predicates"
      (lambda (t)
        (t.is (regex? /foo/) #t)
        (t.is (boolean? nil) #f)
        (t.is (boolean? null) #f)
        (t.is (boolean? undefined) #f)
        (t.is (boolean? #t) #t)
        (t.is (boolean? #f) #t)))

(test "std: find"
      (lambda (t)
        (t.is (find odd? (list 1 2 3)) 1)
        (t.is (find /^[0-9]+$/ (list "foo" "bar" "10")) "10")
        (t.is (to.throw (find "xxx" (list 1 2 3))) true)
        (t.is (find odd? (list 0 2 4 3)) 3)
        (t.is (find odd? (list 0 2 4 6)) nil)))

(test "std: typecheck"
      (lambda (t)
        (t.is (to.throw (typecheck "test" 10 (list "string"))) true)
        (t.is (try (typecheck "test" 10 (list "string") 0) (catch (e) e.message))
              "Expecting string, got number in expression `test` (argument 0)")
        (t.is (try (typecheck "test" 10 (list "string" "character") 0) (catch (e) e.message))
              "Expecting string or character, got number in expression `test` (argument 0)")))
