
(test "formatter: let+if+begin"
      (lambda (t)
        (t.snapshot (pretty-format '(let ((x 10))
                                      (if (zero? x)
                                          (begin
                                            (display "zero")
                                            (newline))
                                          (begin
                                            (display "not zero")
                                            (newline))))))))


(test "formatter: named let"
      (lambda (t)
        (t.snapshot (pretty-format '(let iter ((x 10))
                                      (if (not (zero? x))
                                          (iter (- x 1))))))))

(test "formatter: let"
      (lambda (t)
        (t.snapshot (pretty-format '(let ((x 10))
                                      (print x)
                                      (print x))))

        (t.snapshot (pretty-format '(let ((x))
                                      10
                                      20
                                      "xx
                                       yy"
                                      (print x)
                                      (print x))))

        (t.snapshot (pretty-format '(let ((bar (foo bar (baz)))
                                          (foo 10))
                                      10
                                      20
                                      "xx
                                       yy"
                                      (foo bar)
                                      (print0))))

        (t.snapshot (pretty-format '(let ((bar)
                                          (foo (baz quux))
                                          (foo 10))
                                      (foo bar)
                                      (print0))))

        (t.snapshot (pretty-format '(let ((foo 10)
                                          (xxx (if (null? rest)
                                                   (current-input-port)
                                                   (car rest)))
                                          (bar foo))
                                      (foo bar)
                                      (print0))))))

(test "formatter: cond"
      (lambda (t)
        (t.snapshot (pretty-format '(cond ((foo 10) 10)
                                          ((foo (bar baz))
                                           (begin
                                             (display "x")
                                             (newline))
                                          ((foo)
                                           (foo bar))))))

        (t.snapshot (pretty-format '(cond ((foo 10) 10)
                                          ((foo)
                                           (begin
                                             (display "x")
                                             (newline)))
                                          ((foo)
                                           (foo bar)))))))

(test "formatter: define"
      (lambda (t)

        (t.snapshot (pretty-format '(define (foo x) (+ x x))))

        (t.snapshot (pretty-format '(define (foo x) "xxx" (+ x x))))

        (t.snapshot (pretty-format '(define foo (lambda (x) (+ x x)))))

        (t.snapshot (pretty-format '(define foo (lambda (x) "xxx" (+ x x)))))

        (t.snapshot (pretty-format '(define foo (list 1 2 3))))

        (t.snapshot (pretty-format '(define (foo x)
                                      "foo
                                       bar"
                                      (+ x x))))

        (t.snapshot (pretty-format '(define foo 10)))))


(test "formatter: syntax-rules"
      (lambda (t)

        (t.snapshot (pretty-format '(syntax-rules () ((_ x) (list x)))))
        (t.snapshot (pretty-format '(syntax-rules () ((_) (if)) ((_ x) (list x)))))
        (t.snapshot (pretty-format '(syntax-rules (==>) ((_ a ==> b) (lambda (x) (and (< x a) (> x b)))))))))

(test "formatter: define-syntax"
      (lambda (t)

        (t.snapshot (pretty-format '(define-syntax foo (syntax-rules () ((_ x) (list x))))))))

(test "formatter: nested list"
      (lambda (t)

        (t.snapshot (pretty-format '((1 2 3) (1 2 3) (1 2 3))))))
