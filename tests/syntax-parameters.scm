(test "syntax-parameters: should define syntax-parameter"
      (lambda (t)
        (define-syntax-parameter return
          (syntax-rules ()
            ((_ . _)
             (syntax-error "return used outside of a lambda^"))))

        (t.is (null? (--> (try (return)
                               (catch (e)
                                      e.message))
                          (match #/^return used outside of a lambda\^/)))
              #f)))

(test "syntax-parameters: should create syntax-rules binding with syntax-parameterize"
      (lambda (t)
        (define-syntax-parameter it (syntax-rules () ((_) "default")))
        (syntax-parameterize
         ((it (syntax-rules ()
                ((_) "hello")))
          (is (syntax-rules ()
                ((_) "world"))))
         (t.is (string-append (it) " " (is))
               "hello world"))))
(test "syntax-parameters: should create anaphofric macro"
      (lambda (t)
        (define-syntax-parameter it (syntax-rules () ((_) "default")))
        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (syntax-parameterize
              ((it (syntax-rules ()
                     ((_) "hello, world"))))
              body ...))))

        (t.is (foo (string-append (it) "!"))
              "hello, world!")))

(test "syntax-parameters: should return default parameter when anaphoric variable is used outside"
      (lambda (t)
        (define-syntax-parameter it (syntax-rules () ((_) "default")))

        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (begin
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((_) "hello world")))))
               body ...))))

        (t.is (foo (it)) "default")))

(test "syntax-parameters: parameters should be local"
      (lambda (t)
        (define void (if #f #f))

        (define-syntax-parameter it (syntax-rules () ((_) "default")))

        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (begin
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((_) "hello world"))))
                void)
               body ...))))

        (t.is (foo (let ((it 10))
                     it))
              10)

        (t.is (let ((it 10))
                (foo it))
              10)))

(test "syntax-parameters: user binding should shadow parameter"
      (lambda (t)

        (define-syntax-parameter it (syntax-rules () ((_) "default")))

        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (begin
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((_) "hello world"))))
                body ...)))))

        (t.is (foo (let ((it 10))
                     it))
              10)

        (t.is (let ((it 10))
                (foo it))
              10)))

(test "syntax-parameters: hygience without syntax-parameterize"
      (lambda (t)
        (t.plan 4)

        (define-syntax-parameter it (syntax-rules () ((_) "default")))

        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (begin
               (t.is (it) "default")
               body ...))))

        (foo (t.is (it) "default"))

        (let ((it 10))
          (foo (t.is it 10)))))

(test "syntax-parameters: hygience with syntax-parameterize"
      (lambda (t)
        (t.plan 4)

        (define-syntax-parameter it (syntax-rules () ((_) "default")))

        (define-syntax bar
          (syntax-rules ()
            ((_ body ...)
             (begin
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((__) "hello world"))))
                (t.is (it) "hello world")
                body ...)))))

        (bar (t.is (it) "hello world"))

        (let ((it 10))
          (bar (t.is it 10)))))
