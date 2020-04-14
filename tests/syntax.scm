(test "syntax-rules: hygiene"
      (lambda (t)
        (define result (let ((f (lambda (x) (+ x 1))))
                          (let-syntax ((f (syntax-rules ()
                                            ((_ x) x)))
                                       (g (syntax-rules ()
                                            ((_ x) (f x)))))
                            (list (f 1) (g 1)))))
         (t.is result '(1 2))))

(test "syntax-rules: lambda"
      (lambda (t)
        (let ()
          (define-syntax foo
            (syntax-rules ()
              ((_ x ...) (lambda x ...))))

          (define-syntax test
            (syntax-rules ()
              ((_ x) (foo (z) (+ x z)))))

          (define add-3 (test (let ((z 1)) (+ z 2))))

          (t.is (list
                  (add-3 3)
                  (let ((z 10))
                    ((test z) 10)))
                '(6 20)))))

(test "syntax-rules: complex hygiene"
      (lambda (t)
        (let ((result (let-syntax
                        ((or (syntax-rules ()
                               ((or) #f)
                               ((or e) e)
                               ((or e1 e2 ...)
                                (let ((temp e1))
                                  (if temp
                                      temp
                                      (or e2 ...)))))))
                        (let ((x #f)
                              (y 7)
                              (temp 8)
                              (let odd?)
                              (if even?))
                          (or x
                              (let temp)
                              (if y)
                              y)))))
        (t.is result 7))))

(test "syntax-rules: let + return symbol"
      (lambda (t)
        (define result (let ((x 'outer))
                         (let-syntax ((m (syntax-rules () ((m) x))))
                           (let ((x 'inner))
                             (m)))))
        (t.is result 'outer)))


(test "syntax-rules: quote expression"
      (lambda (t)
          (define-syntax stest
            (syntax-rules ()
              ((_ v . rest) '(default v))))


          (let-syntax ((stest (syntax-rules ()
                                ((_ v . rest) (cons 'v (stest . rest)))
                                ((_) '()))))
            (t.is (stest 5 4 3 2 1 0) '(5 default 4)))

          (letrec-syntax ((stest (syntax-rules ()
                                   ((_ v . rest) (cons 'v (stest . rest)))
                                   ((_) '()))))
            (t.is (stest 5 4 3 2 1 0) '(5 4 3 2 1 0)))))

(test "syntax-rules: recursive or"
      (lambda (t)
         (define or_ (syntax-rules ()
                ((or) #f)
                ((or e) e)
                ((or e1 e2 ...)
                 (let ((temp e1))
                   (if temp
                       temp
                       (or_ e2 ...))))))

         (t.is (or_ #f #f #f #f 10) 10)
         (t.is (or_ #t #f #f) #t)
         (t.is (or_ 10) 10)
         (t.is (or_) #f)))

(test "syntax-rules: rest (dot)"
      (lambda (t)
        (define result (let-syntax ((when (syntax-rules ()
                                            ((when test stmt1 . stmt2)
                                             (if test
                                                 (begin stmt1
                                                        . stmt2))))))
                         (define if #t)
                         (when if (set! if 'now) if)))
        (t.is result 'now)))

(test_ "syntax-rules: ellipsps"
      (lambda (t)
        (define result (let-syntax ((when (syntax-rules ()
                                            ((when test stmt1 stmt2 ...)
                                             (if test
                                                 (begin stmt1
                                                        stmt2 ...))))))
                         (let ((if #t))
                           (when if (set! if 'now))
                           if)))
        (t.is result 'now)))

(test "syntax-rules: function and macro"
      (lambda (t)
        (define even?
          (lambda (x)
            (or (= x 0) (odd? (- x 1)))))
        (define odd?
          (syntax-rules ()
            ((_ x) (not (even? x)))))
        (t.is (even? 10) #t)
        (t.is (even? 13) #f)))

(test "syntax-rules: scope"
      (lambda (t)
        (let ()
         (define-syntax nil!
            (syntax-rules ()
              ((_ x)
               (set! x '()))))

          (let ((set! (lambda (x . rest) x))
                (x 10))
            (nil! x)
            (t.is x nil)))))


