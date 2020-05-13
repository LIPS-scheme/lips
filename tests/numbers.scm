(test "numbers: operation without args"
      (lambda (t)
        (t.is (+) 0)
        (t.is (*) 1)
        (t.is (-) -1)))

(test "numbers: rational"
      (lambda (t)
        (let* ((a 1/2) (b 1/4) (a+b (+ a b)))
          (t.is (= a+b 3/4) true)
          (t.is (number->string a+b) "3/4")
          (t.is (number->string (+ 1/2 1/2)) "1")
          (t.is (number->string (+ 1/2 1/2 1.0)) "2.0")
          (t.is (number->string (+ 1/2 1/2 1/2)) "3/2"))))

(test "numbers: complex rational"
      (lambda (t)

        (t.is  (/ (make-rectangular 1 2) (make-rectangular 2 10))
               (/ 1+2i 2+10i))))

(test "numbers: complex"
      (lambda (t)
        (t.is (sqrt -1) +1.0i)
        (t.is (sqrt 0.5) 0.7071067811865476)
        (t.is (sqrt -0.5) +0.7071067811865476i)
        (t.is (number->string 1e+1i) "+10.0i")
        (t.is (number->string .1i) "+0.1i")
        (t.is (number->string (/ 10+1i +10i)) "1/10-1i")
        (t.is (number->string (make-rectangular 1/2 2/4)) "1/2+1/2i")
        (t.is (/ (make-rectangular 1 2) (make-rectangular 2 10)) 11/52-3/52i)
        (t.is (number->string #e0.5+0.5i) "1/2+1/2i")
        (t.is (number->string #i1/2+2/4i) "0.5+0.5i")
        (t.is (number->string #e0.5+0.1i) "1/2+1/10i")
        (t.is (< 0.1i 0.2i) true)
        (t.is (> 0.1i 0.2i) false)))

(test "numbers: modulo functions"
      (lambda (t)
        (t.is (modulo 13 4) 1)
        (t.is (remainder 13 4) 1)

        (t.is (modulo -13 4) 3)
        (t.is (remainder -13 4) -1)

        (t.is (modulo 13 -4) -3)
        (t.is (remainder 13 -4) 1)

        (t.is (modulo -13 -4) -1)
        (t.is (remainder -13 -4) -1)

        (t.is (remainder -13 -4.0) -1.0)))

(test "numbers: matrix"
      (lambda (t)

        (t.is (+ 1 1.0 1/10) 2.1)
        (t.is (number->string (+ 1/2 1/2 1.0)) "2.0")))
