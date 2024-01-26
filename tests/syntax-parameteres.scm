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
        (syntax-parameterize
         ((it (syntax-rules ()
                ((_) "hello")))
          (is (syntax-rules ()
                ((_) "world"))))
         (t.is (string-append (it) " " (is))
               "hello world"))))
(test "syntax-parameters: should create anaphofric macro"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (syntax-parameterize
              ((it (syntax-rules ()
                     ((_) "hello, world"))))
              body ...))))

        (t.is (foo (string-append (it) "!"))
              "hello, world!")))

(test.failing "syntax-parameters: should throw an error when anaphoric variable is used outside"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (begin
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((_) "hello world")))))
               body ...))))
        (t.is (null? (--> (try (foo (it))
                               (catch (e)
                                      e.message))
                          (match #/Error: Unbound variable `it' in macro:/)))
              #f)))

(test "syntax-parameters: parameters should be local"
      (lambda (t)
        (define void (if #f #f))

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

(test.failing "syntax-parameters: user binding should shadow parameter"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ body ...)
             (begin
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((_) "hello world"))))
                body ...)))))

        (t.is (foo (let ((it 10))
                     (print it)))
              10)

        (t.is (let ((it 10))
                (foo it))
              10)))
