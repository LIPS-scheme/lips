
(test "scope: letrec"
      (lambda (t)
        (let ((x 5))
          (letrec ((foo (lambda (y) (bar x y)))
                   (bar (lambda (a b) (+ (* a b) a))))
            (t.is (foo (+ x 3)) 45)))))


(test "scope: let - should throw"
      (lambda (t)
        (t.is (to.throw (let ((a 10)
                              (b (+ a 10)))
                          (+ b 10)))
              true)))

(test "scope: letrec should calculate !10"
      (lambda (t)
        (t.is (letrec ((x (lambda (y)
                            (if (<= y 0)
                                1
                                (* y (x (- y 1)))))))
                (x 10))
              3628800)))

(test "scope: let* - should throw calling !"
      (lambda (t)
        (t.is (to.throw (let* ((x (lambda (y)
                                    (if (<= y 0)
                                        1
                                        (* y (x (- y 1)))))))
                          (x 10)))
              #t)))

