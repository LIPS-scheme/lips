(test.failing "continuations: base"
  (lambda (t)
    (define x 0)

    (t.is (+ 2 (call/cc (lambda (cc)
                           (set! x cc)
                            3)))
           5)

    (t.is (x 4) 6)))

(test.failing "continuations: return"
       (lambda (t)
          (let ((called #f))

            (define (bar)
              (set! called #t))

            (define (foo)
               (call/cc (lambda (return)
                          (return 10)
                          (bar))))

            (t.is (foo) 10)
            (t.is called #f))))

(test.failing "continuations: calling"
       (lambda (t)
          (let ((called))
            (t.is (let ((my-val (call/cc (lambda (c) c))))
                    (if (procedure? my-val)
                        (my-val 10)
                        (begin
                            (set! called #t)
                            my-val)))
                  10)
             (t.is called #t))))
