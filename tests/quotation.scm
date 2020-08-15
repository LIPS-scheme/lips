(test_ "quasiquote: it should double splice the list"
       (lambda (t)
         (define result (eval (let ((x '((list 1 2 3) (list 1 2 3) (list 1 2 3))))
            `(list `(,@,@x)))
             (interaction-environment)))

         (t.is result '((1 2 3 1 2 3 1 2 3)))))

(test_ "quasiquote: it should backquote & unquote multiple times"
       (lambda (t)
         (define result `(```,,,,@(list 1 2)))
         (t.is result '((quasiquote (quasiquote (quasiquote (unquote (unquote (unquote 1 2))))))))))

(test "quasiquote: it should return list"
      (lambda (t)
        (t.is (eval (let ((x '((list 1 2 3) (list 1 2 3) (list 1 2 3))))
                      `(list `(,@,(car x)))))
              '((1 2 3)))))

(test "quasiquote: single unquote"
      (lambda (t)

        (define result `(let ((name 'x)) `(let ((name 'y)) `(list ',name))))

        (t.is result '(let ((name (quote x)))
                        (quasiquote (let ((name (quote y)))
                                      (quasiquote (list (quote (unquote name))))))))))

(test "quasiquote: double backquote and unquote on list"
      (lambda (t)
        (define result (eval (let ((x '((list 1 2 3) (list 1 2 3) (list 1 2 3))))
                               `(list `(,,@x)))))

        (t.is result '(((1 2 3) (1 2 3) (1 2 3))))))

(test "quasiquote: quaisquote quoted unquoted function"
      (lambda (t)

        (define (foo) 'bar)

        ;; `',(foo) came from book "On Lips" by Paul Graham
        (t.is `',(foo) '(quote bar))))

(test "quasiquote: unquote simple and double unquote symbol"
      (lambda (t)

        (define result (let ((y 20))
                         `(let ((x 10))
                            `(list ,x ,,y))))

        (t.is result '(let ((x 10))
                        (quasiquote (list (unquote x) (unquote 20)))))))

(test "quasiquote: should join symbol"
      (lambda (t)

        (define result (let ((x 'foo)) `(a ,@x)))

        (t.is result '(a . foo))))

(test "quasiquote: should unquote from double quotation"
      (lambda (t)

        (define result (let ((x '(1 2)))
                         `(let ((x '(2 3)))
                            (begin
                              `(list ,(car x))
                              `(list ,,(car x))))))

        (t.is result '(let ((x (quote (2 3))))
                        (begin
                          (quasiquote (list (unquote (car x))))
                          (quasiquote (list (unquote 1))))))))

(test "quasiquote: evaluate unquote inside unquote-splice and double quasiquote"
      (lambda (t)

        (define-macro (bar)
          (let ((x 10) (y '(1 2 3)) (z 'foo))
            `(list ,x `(,@(list ',y)))))

        (t.is (bar) '(10 ((1 2 3))))))

(test "quasiquote: evaluate unquote quote unquote inside double quasiquote"
      (lambda (t)

        (define-macro (bar)
          (let ((x 10) (y '(1 2 3)) (z 'foo))
            `(list ,x `(,',z ,,@y))))

        (t.is (bar) '(10 (foo 1 2 3)))))

(test "quasiquote: evaluate nested quasiquote and unquote with bare list"
      (lambda (t)

        (define-macro (bar)
          (let ((x 10) (y '(1 2 3)) (z 'foo))
            `(list ,x `(,',z `(list ,,,@y)))))

        (define (foo x) x)

        (t.is (eval (cadr (bar))) '(list 1 2 3))))

(let ((fun (lambda (a b)
          (if (number? a)
              (+ a b)
              (if (string? a)
                  (string-append a b)))))
      (f2 (lambda (a b) (list a b)))
      (rand (Math.random)))

  (test "quasiquote: create list with function call"
        (lambda (t)

          (t.is `(1 2 3 ,(fun 2 2) 5) '(1 2 3 4 5))))

  (test "quasiquote: create list with value"
        (lambda (t)

          (t.is `(1 2 3 ,value 4) (list 1 2 3 value 4))))

  (test "quasiquote: create single list using uquote-splice"
        (lambda (t)

          (t.is `(1 2 3 ,@(f2 4 5) 6) '(1 2 3 4 5 6))))

  (test "quasiquote: create single pair"
        (lambda (t)

          (define specs (list
                         `(1 . 2)
                         `(,(car (list 1 2 3)) . 2)
                         `(1 . ,(cadr (list 1 2 3)))
                         `(,(car (list 1 2 3)) . ,(cadr (list 1 2 3)))))

          (define pair '(1 . 2))

          (let iter ((specs specs))
            (if (not (null? specs))
                (begin
                  (t.is (car specs) pair)
                  (iter (cdr specs)))))))

  (test "quasiquote: create list from pair syntax"
        (lambda (t)

          (define result `(,(car (list 1 2 3)) . (1 2 3)))

          (t.is result (list 1 1 2 3))))

  (test "quasiquote: create alist with values"
        (lambda (t)
          (define result  `((1 . ,(car (list 1 2)))
                            (2 . ,(cadr (list 1 "foo")))))

          (t.is result '((1 . 1) (2 . "foo")))

          (define result `((,(car (list "foo")) . ,(car (list 1 2)))
                           (2 . ,(cadr (list 1 "foo")))))

          (t.is result '(("foo" . 1) (2 . "foo")))))

  (test "quasiquote: process nested backquote"
        (lambda (t)

          (define result `(1 2 3 ,(cadr `(1 ,(concat "foo" "bar") 3)) 4))

          (t.is result '(1 2 3 "foobar" 4))))

  (test "quasiquote: should process multiple backquote/unquote"
        (lambda (t)

          (define result ``(a ,,(+ 1 2) ,(+ 3 4)))

          (t.is result '(quasiquote (a (unquote 3) (unquote (+ 3 4))))))))

(test "quasiquote: should ignore splice on empty list"
      (lambda (t)
        (define result `(list ,@(list)))

        (t.is result '(list))))

(test "quote: should return literal list"
      (lambda (t)

        (t.is '(1 2 3 (4 5)) (list 1 2 3 (list 4 5)))))

(test "quote: should return alist"
      (lambda (t)

        (t.is '((foo . 1)
                (bar . 2.1)
                (baz . "string")
                (quux . /foo./g))

              (list (cons 'foo 1)
                    (cons 'bar 2.1)
                    (cons 'baz "string")
                    (cons 'quux /foo./g)))))

(test "quote: should return literal atoms"
      (lambda (t)

        (t.is (list '#f
                    '#t
                    '10
                    '10+10i
                    '#x10
                    '#x#i10
                    '#e#x10
                    '#e#o7
                    '#\x
                    '#\newline)

              (list #f
                    #t
                    10
                    10+10i
                    #x10
                    #x#i10
                    #e#x10
                    #e#o7
                    #\x
                    #\newline))))
