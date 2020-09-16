
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



(test "formatter: let + while"
      (lambda (t)
        (t.snapshot (pretty-format '(let ((x 10))
                                      (while (> x 0)
                                        (display x)
                                        (newline)
                                        (-- x)))))))

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
