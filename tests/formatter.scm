
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

        (t.snapshot (pretty-format '(define foo (lambda (x) (+ x x)))))

        (t.snapshot (pretty-format '(define foo (list 1 2 3))))

        (t.snapshot (pretty-format '(define foo 10)))))


(test "formatter: syntax-rules"
      (lambda (t)

        (t.snapshot (pretty-format '(syntax-rules () ((_ x) (list x)))))
        (t.snapshot (pretty-format '(syntax-rules () ((_) (if)) ((_ x) (list x)))))
        (t.snapshot (pretty-format '(syntax-rules (==>) ((_ a ==> b) (lambda (x) (and (< x a) (> x b)))))))))

(test "formatter: define-syntax"
      (lambda (t)

        (t.snapshot (pretty-format '(define-syntax foo (syntax-rules () ((_ x) (list x))))))))
