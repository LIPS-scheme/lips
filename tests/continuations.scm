(test_ "continuations"
  (lambda (t)
    (define x 0)

    (t.is (+ 2 (call/cc (lambda (cc)
                           (set! x cc)
                            3)))
           5)

    (t.is (x 4) 6)))
