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
