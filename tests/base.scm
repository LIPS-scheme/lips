(test "timing test"
      (lambda (t)
        (--> t (is (function? (.. Date.now)) true))
        (define start (--> Date (now)))
        (delay 100 (--> t (is (>= (- (--> Date (now)) start) 100) true)))))
