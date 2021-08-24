(test "macroexpand: should not expand let bindings"
      (lambda (t)
        (t.snapshot (macroexpand (let ((++ (lambda (a b) (* a b)))) (++ 1 2))))
        (t.snapshot (macroexpand (let ((++ 10)) (+ ++ ++))))))

(test "macroexpand: should only expand macro in body of let"
      (lambda (t)
        (define-macro (foo x) `(1 2 3 ,x))
        (define-macro (bar x) `(x y z ,x))

        (t.snapshot (macroexpand (let ((foo 10)) (bar foo))))))

(test "macroexpand: should not expand lambda parameters"
      (lambda (t)
        (define-macro (foo x) `(1 2 3 ,x))
        (define-macro (bar x) `(x y z ,x))

        (t.snapshot (macroexpand (lambda (foo) (bar foo))))))

(test "macroexpand: should not expand shadowed procedure name in recursive call"
      (lambda (t)
        (define-macro (foo x) `(1 2 3 ,x))
        (define-macro (bar x) `(x y z ,x))

        (t.snapshot (macroexpand (define (foo x) (if (zero? x) (bar 0) (foo (- x 1))))))))

(test "macroexpand: should expand macro inside letrec function"
      (lambda (t)
        (define-macro (foo x) `(1 2 3 ,x))
        (define-macro (bar x) `(x y z ,x))

        (t.snapshot (macroexpand (letrec ((foo (lambda () (bar x)))) (foo 10))))))
