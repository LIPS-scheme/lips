(test "core: it should set!/set-obj! with this and prototype"
      (lambda (t)
        (let ()
          (define foo (lambda (x) (set! this.x x)))
          (define bar (new foo 10))
          (set! foo.prototype.square (lambda (x) (* x x)))
          (set! foo.prototype.sum (lambda (x) (+ this.x x)))
          (t.is (bar.square 10) 100)
          (t.is (bar.sum 5) 15))
        (let ()
          (define foo (lambda (x) (set-obj! this "x" x)))
          (define bar (new foo 10))
          (set-obj! foo.prototype 'square (lambda (x) (* x x)))
          (set-obj! foo.prototype 'sum (lambda (x) (+ this.x x)))
          (t.is (bar.square 10) 100)
          (t.is (bar.sum 5) 15))))

(test "core: it should set on object literals"
      (lambda (t)
        (let ((x &(:foo "jo")))
          (set! x.bar "hey")
          (t.is (string-append (--> x.bar (toUpperCase))
                               " "
                               (x.foo.toUpperCase))
                "HEY JO"))))

(test "timing test"
      (lambda (t)
        (--> t (is (function? (.. Date.now)) true))
        (define start (--> Date (now)))
        (wait 100 (--> t (is (>= (- (--> Date (now)) start) 100) true)))))

(test "values"
      (lambda (t)
        (t.is (call-with-values * -) -1)
        (t.is (call-with-values (lambda () (values 4 5))
                (lambda (a b) b)) 5)
        (t.is (call-with-values (lambda () (values 4 5)) +) 9)))

(test "symbols"
      (lambda (t)
        (t.is '|foo\x20;bar| (string->symbol "foo bar"))
        (t.is '|\n| (string->symbol "\n"))
        (t.is '|\t\t| (string->symbol "\t\t"))
        (t.is '|\r| (string->symbol "\r"))
        (t.is '|\s| 's)
        (t.is '|\x3BB;| 'λ)
        (t.is '|\x9;\x9;| '|\t\t|)))
