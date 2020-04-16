


(test "timing test"
      (lambda (t)
        (--> t (is (function? (.. Date.now)) true))
        (define start (--> Date (now)))
        (delay 100 (--> t (is (>= (- (--> Date (now)) start) 100) true)))))

(test "values"
      (lambda (t)
        (t.is (call-with-values * -) -1)
        (t.is (call-with-values (lambda () (values 4 5))
                (lambda (a b) b)) 5)
        (t.is (call-with-values (lambda () (values 4 5)) +) 9)))
