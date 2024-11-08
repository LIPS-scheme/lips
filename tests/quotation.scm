(test "quasiquote: it should splice an empty list"
      (lambda (t)
        (t.is `(x ,@() x) '(x x))))

(test "quasiquote: it should splice nil as body in do macro"
      (lambda (t)
        (define-macro (do vars test . body)
          "(do ((<var> <init> <next>)) (test expression) . body)

           Iteration macro that evaluate the expression body in scope of the variables.
           On Eeach loop it increase the variables according to next expression and run
           test to check if the loop should continue. If test is single call the macro
           will not return anything. If the test is pair of expression and value the
           macro will return that value after finish."
          (let ((return? (eq? (length test) 2)) (loop (gensym)))
            `(let ,loop (,@(map (lambda (spec)
                                  `(,(car spec) ,(cadr spec)))
                                vars))
                  (if (not ,(car test))
                      (begin
                        ,@body
                        (,loop ,@(map (lambda (spec)
                                        (if (null? (cddr spec))
                                            (car spec)
                                            (caddr spec)))
                                      vars)))
                      ,(if return? (cadr test))))))

        ;; nil as body
        (t.is (let ((x '(1 3 5 7 9)))
                (do ((x x (cdr x))
                     (sum 0 (+ sum (car x))))
                  ((null? x) sum))) 25)

        ;; ignored body
        (t.is (let ((x '(1 3 5 7 9)))
                (do ((x x (cdr x))
                     (sum 0 (+ sum (car x))))
                  ((null? x) sum) 10)) 25)))

(test "quasiquote: it should double splice the list"
       (lambda (t)

         (define expr (let ((x '((list 1 2 3) (list 4 5 6) (list 7 8 9))))
                                `(list `(,@,@x))))

         (t.is expr '(list (quasiquote ((unquote-splicing (list 1 2 3)
                                                          (list 4 5 6)
                                                          (list 7 8 9))))))
         (define result (eval expr (interaction-environment)))

         (t.is result '((1 2 3 4 5 6 7 8 9)))))

(test "quasiquote: it should backquote & unquote multiple times"
      (lambda (t)
        (define result `(```,,,,@(list 1 2)))
        (t.is result '((quasiquote (quasiquote (quasiquote (unquote (unquote (unquote 1 2))))))))))

(test "quasiquote: it should quasiquote unquote-splice"
      (lambda (t)

        (t.is `(1 ```,,@,,@(list (+ 1 2)) 4)
              '(1 (quasiquote (quasiquote (quasiquote (unquote (unquote-splicing (unquote 3)))))) 4))

        (t.is `(1 `,@(list (+ 1 2)) 4)
              '(1 (quasiquote (unquote-splicing (list (+ 1 2)))) 4))))

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
        (define result (eval (let ((x '((list 1 2 3) (list 4 5 6) (list 7 8 9))))
                               `(list `(,,@x)))
                             (interaction-environment)))

        (t.is result '(((1 2 3) (4 5 6) (7 8 9))))))

(test "quasiquote: quaisquote quote unquoted"
      (lambda (t)

        (define (foo) 'bar)

        ;; `',(foo) came from book "On Lips" by Paul Graham
        (t.is `',(foo) '(quote bar))
        (let ((x 'foo))
          (t.is `',x '(quote foo)))))

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

        (t.is (eval (cadr (bar)) (current-environment)) '(list 1 2 3))))

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
                (quux . #/foo./g))

              (list (cons 'foo 1)
                    (cons 'bar 2.1)
                    (cons 'baz "string")
                    (cons 'quux #/foo./g)))))

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

(test "quote: should be constant"
      (lambda (t)

        (define (foo)
          '(1 2))

        (t.is (eq? (foo) (foo)) true)))

(test "quasiquote: should be constant"
      (lambda (t)

        (define (foo)
          `(1 2))

        (t.is (eq? (foo) (foo)) true)))

(test "quasiquote: should create new pair"
      (lambda (t)
        (define (foo x)
          `(1 2 ,@x))

        (t.is (eq? (foo '(1)) (foo '(2))) false)

        (define (foo x)
          `(1 2 ,x))

        (t.is (eq? (foo 10) (foo 20)) false)))

(test "quasiquote: should crete vector literal"
      (lambda (t)
        (t.is `#(,(+ 1 2) ,(+ 2 3) ,(Promise.resolve 7))
              #(3 5 7))))

(test "quasiquote: should crete object literal"
      (lambda (t)
        (t.is `&(:foo ,(+ 1 2) :bar ,(Promise.resolve 10))
              &(:foo 3 :bar 10))))

(test "quasiquote: should create vector inside list"
      (lambda (t)
        (t.is `(foo #(10 ,@(list 1 2 3)))
              '(foo #(10 1 2 3)))))

(test "quasiquote: should create object inside list"
      (lambda (t)
        (t.is `(foo &(:foo ,(+ 1 2) :bar 10))
              (list 'foo &(:foo 3 :bar 10)))))

(test "quasiquote: should create list from improper list"
      (lambda (t)
        (t.is (let ((x '(1 2 3)))
                `(foo . ,x))
              '(foo 1 2 3))))

(test "quasiquote: should create list with unquote-splicing and improper list"
      (lambda (t)
        (let ((result `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))))
          (t.is result '((foo 7) . cons)))))

(test "quasiquote: should process list after double unquote-splicing (#362)"
      (lambda (t)
        (let ((x '(1 2 3))
              (y '(11 22 33))
              (l '(x y)))
          (t.is ``(,@,@l ,@,@l)
                '(quasiquote ((unquote-splicing x y) (unquote-splicing x y))))
          (t.is (eval ``(foo ,@,@l ,@,@l bar) (current-environment))
                '(foo 1 2 3 11 22 33 1 2 3 11 22 33 bar)))))
