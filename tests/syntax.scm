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

(test "syntax-rules: double splice"
      (lambda (t)

        (define-syntax foo
          (syntax-rules ()
            ((foo (f1 ...) (f2 ...) . body-forms)
             '(f1 ... f2 ... . body-forms))))

        (t.is (foo (a b c d) (1 2 3 4) moe larry curly)
              '(a b c d 1 2 3 4 moe larry curly))))


(test "syntax-rules: when syntax hygiene"
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

(test "syntax-rules: skip second item in list"
   (lambda (t)
     (define-syntax foo
       (syntax-rules () ((_ (a . (b . (c ...))) ...) '(foo (a c ... ) ...))))
     (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (1 3 4 5) (6 8 9 10)))))

(test "syntax-rules: only cddr (list)"
   (lambda (t)

     (define-syntax foo
       (syntax-rules () ((_ (a b c ...) ...) '(foo (c ...) ...))))

     (t.is (foo) '(foo))
     (t.is (to.throw (foo 1)) #t)
     (t.is (to.throw (foo (1))) #t)
     (t.is (foo (1 2)) '(foo ()))
     (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (3 4 5) (8 9 10)))))

(test "syntax-rules: only cddr (cons literals)"
   (lambda (t)

     (define-syntax foo
       (syntax-rules () ((_ (a . (b . (c ...))) ...) '(foo (c ...) ...))))

     (t.is (foo) '(foo))
     (t.is (to.throw (foo 1)) #t)
     (t.is (to.throw (foo (1))) #t)
     (t.is (foo (1 2)) '(foo ()))
     (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (3 4 5) (8 9 10)))))

(test "syntax-rules: map on cddr"
   (lambda (t)

      (define-syntax foo
         (syntax-rules () ((_ x ...) (cons 'foo (map cddr '(x ...))))))

      (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (3 4 5) (8 9 10)))))

(test "syntax-rules: extract 1st and 2nd items from list"
   (lambda (t)

      (define-syntax foo
         (syntax-rules () ((_ (a . (b . (c . nil))) ...) '(foo (a . c) ...))))

      (t.is (foo) '(foo))
      (t.is (foo (1 2 3)) '(foo (1 . 3)))
      (t.is (foo (1 2 3) (4 5 6)) '(foo (1 . 3) (4 . 6)))))

(test "syntax-rules: extract 2nd elements from lists"
   (lambda (t)

      (define-syntax foo
         (syntax-rules () ((_ (a . (b . (c . nil))) ...) '(foo b ...))))

      (t.is (foo) '(foo))
      (t.is (to.throw (foo 1)) #t)
      (t.is (to.throw (foo (1))) #t)
      (t.is (to.throw (foo (1 2))) #t)
      (t.is (foo (1 2 3)) '(foo 2))
      (t.is (foo (1 2 3) (4 5 6)) '(foo 2 5))
      (t.is (foo (1 2 3) (4 5 6) (7 8 9)) '(foo 2 5 8))))

(test "syntax-rules: should spread elements"
   (lambda (t)

      (define-syntax foo
         (syntax-rules ()
           ((_ (a . (b . (c . nil))) ...)
            '(foo a ... b ... c ...))))

      (t.is (foo) '(foo))
      (t.is (to.throw (foo 1)) #t)
      (t.is (to.throw (foo (1))) #t)
      (t.is (to.throw (foo (1 2))) #t)
      (t.is (foo (1 2 3) (4 5 6) (7 8 9)) '(foo 1 4 7 2 5 8 3 6 9))
      (t.is (foo (1 2 3)) '(foo 1 2 3))
      (t.is (foo (1 2 3) (4 5 6)) '(foo 1 4 2 5 3 6))))

(test "syntax-rules: list quine"
  (lambda (t)
    (define-syntax foo
      (syntax-rules ()
        ((_ (x ...) ...)
         '(foo (x ...) ...))))

    (t.is (foo) '(foo))
    (t.is (to.throw (foo 1)) #t)
    (t.is (foo ()) '(foo ()))
    (t.is (foo (x)) '(foo (x)))
    (t.is (foo (x y)) '(foo (x y)))
    (t.is (foo (a b) (c d)) '(foo (a b) (c d)))))

(test "syntax-rules: cons 1st and 2nd in lists"
  (lambda (t)

    (define-syntax foo
       (syntax-rules () ((_ (a b) ...)  '((a . b) ...))))

    (t.is (foo) '())
    (t.is (to.throw (foo 1)) #t)
    (t.is (to.throw (foo ())) #t)
    (t.is (to.throw (foo (1))) #t)

    (t.is (foo (1 2)) '((1 . 2)))
    (t.is (foo (1 2) (3 4)) '((1 . 2) (3 . 4)))
    (t.is (foo (1 2) (3 4) (5 6)) '((1 . 2) (3 . 4) (5 . 6)))))

(test "syntax-rules: zip trasformation"
  (lambda (t)

    (define-syntax foo
       (syntax-rules () ((_ (a ...) (b ...)) '((a . b) ...))))

    (t.is (to.throw (foo)) #t)
    (t.is (to.throw (foo 1)) #t)
    (t.is (to.throw (foo 1 1)) #t)
    (t.is (to.throw (foo (1))) #t)
    (t.is (to.throw (foo () () ())) #t)
    (t.is (foo (1) (2)) '((1 . 2)))
    (t.is (foo (1 2) (3 4)) '((1 . 3) (2 . 4)))
    (t.is (foo (1 2 3) (4 5 6)) '((1 . 4) (2 . 5) (3 . 6)))))

(test "syntax-rules: merge lists"
      (lambda (t)
        (define-syntax merge
          (syntax-rules ()
            ((_) '())
            ((_ (foo ...) . rest)
             (append (list foo ...) (merge . rest)))))

        (t.is (to.throw (merge 1)) #t)
        (t.is (to.throw (merge 1 2)) #t)
        (t.is (merge) '())
        (t.is (merge (1 2 3)) '(1 2 3))
        (t.is (merge (1 2 3) (4 5 6)) '(1 2 3 4 5 6))
        (t.is (merge (1 2 3) (4 5 6) (7 8 9)) '(1 2 3 4 5 6 7 8 9))))

(test "syntax-rules: identifiers"
      (lambda (t)
        (define-syntax let+
          (syntax-rules (==>)
            ((_ ((a ==> b) ...) . body)
             (let ((a b) ...) . body))))

        (t.is (let+ ((a ==> 1)
                     (b ==> 2))
                    (+ a b))
              3)))

(test "syntax-rules: basic ellipsis (srfi-46)"
      (lambda (t)

        (define-syntax funcall
          (syntax-rules ::: ()
             ((_ name args :::) (name args :::))))

        (t.is (funcall list 1 2 3) '(1 2 3))))

(test "syntax-rules: macro define function"
      (lambda (t)

        (define-syntax def
          (syntax-rules (==>)
            ((_ name ==> body ...)
             (define name (lambda body ...)))))

        (def square ==> (x) (* x x))
        (t.is (square 10) 100)))

(test "syntax-rules: macro define list of functions"
      (lambda (t)

        (define-syntax defn
          (syntax-rules (==>)
            ((_ (name ==> body ...) ...)
             (begin
               (define name (lambda body ...))
               ...))))

        (defn (square ==> (x) (* x x))
              (add ==> (a b) (+ a b))
              (sum ==> a (apply + a)))
        (t.is (square (add 6 4)) 100)
        (t.is (sum 1 2 3) 6)))

(test "syntax-rules: nested syntax-rules (srfi-46)"
      (lambda (t)

        (define-syntax list+
          (syntax-rules ::: ()
             ((_ args :::) '(args :::))))

        (define-syntax alias
          (syntax-rules ()
            ((_ name ref)
             (define-syntax name
               (syntax-rules ::: ()
                 ((_ args :::)
                  (ref args :::)))))))

        (alias list- list+)
        (t.is (list+ 1 2 3) '(1 2 3))
        (t.is (list- 4 5 6) '(4 5 6))))

(test "syntax-rules: nested syntax-rules gensyms (srfi-46)"
      (lambda (t)

        (define result (let-syntax
                          ((f (syntax-rules ()
                                ((f ?e)
                                 (let-syntax
                                     ((g (syntax-rules ::: ()
                                           ((g (??x ?e) (??y :::))
                                            '((??x) ?e (??y) :::)))))
                                   (g (1 2) (3 4)))))))
                        (f :::)))

        (t.is result '((1) 2 (3) (4)))))

(test "syntax-rules: tail of ellipsis (srfi-46)"
      (lambda (t)

        (define result (let-syntax
                          ((foo (syntax-rules ()
                                  ((foo ?x ?y ... ?z)
                                   (list ?x (list ?y ...) ?z)))))
                        (foo 1 2 3 4 5)))

        (t.is result '(1 (2 3 4) 5))

        (define result (let-syntax
                          ((foo (syntax-rules ()
                                  ((foo ?a ?b ... ?c ?d)
                                   (list ?a (list ?b ...) ?c ?d)))))
                        (foo 1 2 3 4 5)))

        (t.is result '(1 (2 3) 4 5))))

(test "syntax-rules: rec macro (srfi-31)"
      (lambda (t)

        (define-syntax rec
          (syntax-rules ()
            ((rec (NAME . VARIABLES) . BODY)
             (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
            ((rec NAME EXPRESSION)
             (letrec ( (NAME EXPRESSION) ) NAME))))

        (define F (rec (F N)
                       ((rec (G K L)
                             (if (zero? K) L
                                 (G (- K 1) (* K L)))) N 1)))


        (t.is (F 10) 3628800)))

(test "syntax-rules: join macros"
      (lambda (t)

        (define-syntax join_1
          (syntax-rules ()
            ((_ (foo ...) . x)
             (list foo ... . x))))

        (t.is (join_1 (1 2 3) 4) '(1 2 3 4))
        (t.is (join_1 (1 2 3) 4 5 6) '(1 2 3 4 5 6))

        (define-syntax join_2
          (syntax-rules ()
            ((_ (foo ...) x)
             (list foo ... x))))

        (t.is (join_2 (1 2 3) 4) '(1 2 3 4))
        (t.is (to.throw (join_2 (1 2 3) 4 5)) #t)))

(test "syntax-rules: double ellipsis"
      (lambda (t)

        (define result (let-syntax
                           ((my-append
                             (syntax-rules ()
                               ((my-append (a ...) ...) '(a ... ...)))))
                         (my-append (1 2 3) (4 5 6))))

        (t.is result '(1 2 3 4 5 6))))

(test_ "syntax-rules: lifted ellipsis"
      (lambda (t)
        (define result
          (let-syntax
              ((foo (syntax-rules ()
                      ((foo (a b ...) ...) '(((a b) ...) ...)))))
            (foo (bar 1 2) (baz 3 4))))

        (t.is result '(((bar 1) (bar 2)) ((baz 3) (baz 4))))))


(test "syntax-rules: R6RS do macro"
       (lambda (t)
         (define-syntax do
           (syntax-rules ()
             ((do ((var init step ...) ...)
                (test expr ...)
                command ...)
              (letrec
                  ((loop
                    (lambda (var ...)
                      (if test
                          (begin
                            #f ; avoid empty begin
                            expr ...)
                          (begin
                            command
                            ...
                            (loop (do "step" var step ...)
                                  ...))))))
                (loop init ...)))
             ((do "step" x)
              x)
             ((do "step" x y)
              y)))

         (t.is (do ((vec (make-vector 5))
                    (i 0 (+ i 1)))
                 ((= i 5) vec)
                 (vector-set! vec i i))
               #(0 1 2 3 4))

         (t.is (let ((x '(1 3 5 7 9)))
                 (do ((x x (cdr x))
                      (sum 0 (+ sum (car x))))
                   ((null? x) sum)))
               25)))

;; foo foo ... should match single element foo ... should match nil
(test "syntax-rules: R6RS unless & when macros"
       (lambda (t)

         (define-syntax when
           (syntax-rules ()
             ((when test result1 result2 ...)
              (if test
                  (begin result1 result2 ...)))))

         (define-syntax unless
           (syntax-rules ()
             ((unless test result1 result2 ...)
              (if (not test)
                  (begin result1 result2 ...)))))


         (t.is (when (> 3 2) 'foo) 'foo)
         (t.is (when (< 3 2) 'foo) undefined) ;; unspecified

         (t.is (unless (> 3 2) 'less) undefined) ;; unspecified

         (t.is (unless (< 3 2) 'foo) 'foo)))

;; guile example
(test "syntax-rules: literal atoms"
       (lambda (t)
          (define-syntax define-matcher-macro
            (syntax-rules ()
              ((_ name lit)
               (define-syntax name
                 (syntax-rules ()
                  ((_ lit) #t)
                  ((_ else) #f))))))

             (define-matcher-macro is-literal-foo? "foo")

             (t.is (is-literal-foo? "foo") #t) ;; this fail
             (t.is (is-literal-foo? "bar") #f)
             (let ((foo "foo"))
                (t.is (is-literal-foo? foo) #f))))

(test "syntax-rules: my-or hygiene"
      (lambda (t)

        (define-syntax my-or
          (syntax-rules ()
            ((my-or)
             #t)
            ((my-or exp)
             exp)
            ((my-or exp rest ...)
             (let ((t exp))
               (if t
                   t
                   (my-or rest ...))))))
         (t.is (let ((t #t)) (my-or #f t)) #t)))


(test "syntax-rules: recursive do"
       (lambda (t)
         (define-syntax do
            (syntax-rules ()
              ((do ((var start inc) ...) (test) body ...)
               (do ((var start inc) ...) (test ()) body ...))
              ((do ((var start inc) ...) (test result) body ...)
               (begin
                  (let iter ((var start) ...)
                    (if test
                        result
                        (begin
                           body ...
                           (iter inc ...))))))))

          (t.is (do ((i 10 (- i 1)))
                    ((zero? i)))
                '())

          ;; working direct matching
          (t.is (let ((result '()))
                  (do ((i 10 (- i 1)))
                      ((zero? i) result)
                      (set! result (cons i result))))
                '(1 2 3 4 5 6 7 8 9 10))))


(test "syntax-rules: it should define nested syntax-rules"
      (lambda (t)
        ;; be-like-begin from R7RS spec file
        (define-syntax be-like-begin
          (syntax-rules ()
            ((be-like-begin name)
             (define-syntax name
               (syntax-rules ()
                 ((name expr (... ...))
                  (begin expr (... ...))))))))

        (be-like-begin sequence)
        (t.is (sequence 1 2 3 4) 4)

        (be-like-begin progn)
        (t.is (let* ((x 10)
                     (expr `(,x . ,x)))
                (progn
                 x
                 x
                 expr))
              '(10 . 10))))

(test "syntax-rules: recursive call"
      (lambda (t)

        (define-syntax L
          (syntax-rules ()
            ((_) '())
            ((_ a b ...) (cons a (_ b ...)))))

        (t.is (L 1 2 3) '(1 2 3))))

(test_ "syntax-rules: should return list with ellipsis"
       (lambda (t)

         (define-syntax test
           (syntax-rules ()
             ((_) (... '...))))

         (t.is (test) '(...))

         (define-syntax test
           (syntax-rules ()
             ((_) (test 1 2))
             ((_ arg ...) (list (cons arg (... '...)) ...))))

         (t.is (test 1 2 3) '((1 . ...) (2 . ...) (3 . ...)))
         (t.is (test) '((1 . ...) (2 . ...)))))


(test "syntax-rules: it should handle identifiers"
       (lambda (t)

         (define-syntax for
           (syntax-rules (in as)
             ((for element in list body ...)
              (map (lambda (element)
                      body ...)
                   list))
            ((for list as element body ...)
             (for element in list body ...))))

         (t.is (let ((result '()))
                 (for i in '(0 1 2 3 4)
                      (set! result (cons i result)))
                  result)
               '(4 3 2 1 0))

         (t.is (let ((result '()))
                 (for '(0 1 2 3 4) as i
                      (set! result (cons i result)))
                 result)
               '(4 3 2 1 0))))


(test "syntax-rules: it should define let*"
      (lambda (t)
        ;; source https://www.scheme.com/tspl2d/syntax.html#g2252
        (t.is (type let*) "macro")
        (define-syntax let*
          (syntax-rules ()
            ((_ () e1 e2 ...) (let () e1 e2 ...))
            ((_ ((i1 v1) (i2 v2) ...) e1 e2 ...)
             (let ((i1 v1))
               (let* ((i2 v2) ...) e1 e2 ...)))))
        (t.is (type let*) "syntax")
        (t.is (let* ()
                (+ 1 2))
              3)
        (t.is (let* ((x 10)
                     (y (+ x 2)))
                (+ x y))
              22)))

(test "syntax: scope + identifiers"
      (lambda (t)

        (define-syntax foo
          (syntax-rules (++)
            ((_ x ++ y)
             (list x 1 1 y))))

        (define (test)
          (let ((__ 10))
            (define-syntax foo
              (syntax-rules (__)
                ((_ x __ y)
                 (list x 1 1 y))))
            (foo 'a __ 2)))

        (t.is (test) (list 'a 1 1 2))

        (define (test)
          (define-syntax foo
            (syntax-rules (__)
              ((_ x __ y)
               (list x 1 1 y))))
          (define __ 10)
          (foo 'b __ 2))

        (t.is (test) (list 'b 1 1 2))

        (t.is (foo 1 ++ 2) (list 1 1 1 2))
        (t.is (to.throw (let ((++ 10))
                          (foo 1 ++ 2)))
              true)
        (t.is (to.throw (let* ((++ 10))
                          (foo 1 ++ 2)))
              true)
        (t.is (to.throw ((lambda (++)
                           (foo 1 ++ 2)) 10))
              true)))

(test "syntax: scope with rewriting"
      (lambda (t)
        ;; ref: https://www.cs.utah.edu/plt/scope-sets/
        (define self (letrec-syntax ([identity (syntax-rules ()
                                                 [(_ misc-id)
                                                  (lambda (x)
                                                    (let ([misc-id 'other])
                                                      x))])])
                       (identity x)))
        (t.is (self 10) 10)

        ;; racket macro
        (define-syntax define-syntax-rule
          (syntax-rules ()
            ((_ (name args ...) body)
             (define-syntax name (syntax-rules () ((name args ...) body))))))

        (define-syntax-rule (define-other-five misc-id)
          (begin
            (define x 5)
            misc-id))

        (t.is (to.throw (define-other-five x)) true)))

(test "syntax: define syntax macro inside syntax-macro"
      (lambda (t)

        (define-syntax def (syntax-rules () ((_ x y ) (define x y))))
        (define-syntax def-2 (syntax-rules () ((_ x y) (def x y))))

        (def foo 10)
        (def-2 bar 20)
        (t.is (+ foo bar) 30)))

(test_ "syntax: free variables"
      (lambda (t)
        (define-syntax def (syntax-rules ()
                             ((_ foo bar)
                              (begin
                                (define foo bar)
                                hello))))

        (t.is (def hello 10) 10)
        (def hello 10)
        (t.is hello 10)))

(test "syntax: it should pass body from macro to function"
      (lambda (t)

        (define-syntax foo
          (syntax-rules ()
            ((_ . bar) (baz . bar))))

        (define (baz . args) args)

        (t.is (foo 1 2 3) '(1 2 3))))

(test "syntax: it should find last item in list"
      (lambda (t)

        (define-syntax last
          (syntax-rules ()
            ((_ ()) ())
            ((_ (x)) x)
            ((_ (x ... y)) y)))

        (t.is (last (1 2 3 4 5 6)) 6)
        (t.is (last (1)) 1)
        (t.is (last ()) ())
        (t.is (to.throw (last)) true))

(test "syntax: it should find last item in argument list"
      (lambda (t)

        (define-syntax last-arg
          (syntax-rules ()
            ((_) ())
            ((_ x) x)
            ((_ x ... y) y)))

        (t.is (last-arg 1 2 3 4 5 6) 6)
        (t.is (last-arg 1) 1)
        (t.is (last-arg) ())
        (t.is (to.throw (last)) true)))


(test "syntax: it should skip cons with identifier"
      (lambda (t)
        (define-syntax foo
          (syntax-rules (<>)
            ((_ <> . b) nil)
            ((_ a . b) (cons a b))))

        (t.is (foo 1) '(1))
        (t.is (foo 1 . 2) '(1 . 2))))

(test "syntax: it should define nested syntax with variable from outside as identifier"
      (lambda (t)

        (define-syntax foo (syntax-rules ()
                     ((_ bar)
                      (let ()
                        (define-syntax baz
                          (syntax-rules (bar)
                            ((_ x) nil)))
                        (baz bar)))))

        (t.is (foo 10) nil)))

(test "syntax: it should expand in nested syntax into variable from parent syntax"
      (lambda (t)
        (define-syntax foo (syntax-rules ()
                     ((_ bar quux)
                      (let ()
                        (define-syntax baz
                          (syntax-rules (bar)
                            ((_ x) (list quux))))
                        (baz bar)))))

        (t.is (foo 1 "hello") '("hello"))))

(test "syntax: it should expand nested macro with ellipsis as identifier from parent"
      (lambda (t)

        (define-syntax foo (syntax-rules (ellipsis)
                             ((_)
                              (let ()
                                (define-syntax foo
                                  (syntax-rules ellipsis ()
                                    ((_ x ellipsis) (list x ellipsis))))
                                (foo 1 2 3)))))

        (t.is (foo) '(1 2 3))

        ;; recursive case
        (define-syntax foo (syntax-rules (ellipsis)
                     ((_)
                      (let ()
                        (define-syntax foo
                          (syntax-rules ellipsis ()
                                        ((_) ())
                                        ((_ x) (list x))
                                        ((_ x ellipsis) (list (foo x) ellipsis))))
                        (foo 1 2 3)))))

        (t.is (foo) '((1) (2) (3)))))

(test "syntax-rules: it should ignore ellipsis in middle for 2 elements"
      (lambda (t)
        ;; code for define-values from R7RS spec
        ;; macro defined in lib/R7RS.scm

        (let ()
          (define-values (x y) (values 1 2))
          (t.is (+ x y) 3))

        (let ()
          (define-values (x y z) (values 1 2 3))
          (t.is (+ x y z) 6))

        (let ()
          (define-values (x) (values 1))
          (t.is x 1))))

(test "syntax: swap macro"
      (lambda (t)
        ;; example from book Sketchy Scheme by Nils M Holm
        (define-syntax swap
          (syntax-rules ()
            ((_ (x y) ...)
             (list (quote (y x)) ...))))

        (t.is (swap) '())
        (t.is (swap (1 2)) '((2 1)))
        (t.is (swap (1 2) (3 4)) '((2 1) (4 3)))))

(test "syntax: reverse-syntax macro"
      (lambda (t)

        (define-syntax reverse-syntax
          (syntax-rules ()
            ((_ lst)
             (reverse-syntax lst ()))
            ((_ () r) r)
            ((_ (a . d) r)
             (reverse-syntax d (a . r)))))

        (t.is (reverse-syntax (1 2 cons)) '(2 . 1))
        (t.is (reverse-syntax (1 2 3 4 5 list)) '(5 4 3 2 1))))

(test "syntax: duplicated expansion"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ (a ...) ...)
             (list (list (list a ...) (list a ...)) ...))))

        (t.is (foo (1 2 3) (4 5 6))
              '(((1 2 3) (1 2 3)) ((4 5 6) (4 5 6))))))

(test "syntax: R7RS multiple elipsis extensions"
      (lambda (t)

        ;; source https://practical-scheme.net/gauche/man/gauche-refe/Hygienic-macros.html
        (define-syntax my-append
          (syntax-rules ()
            [(_ (a ...) ...)
             '(a ... ...)]))

        (t.is (my-append (1 2 3) (4) (5 6)) '(1 2 3 4 5 6))

        (define-syntax my-append2
          (syntax-rules ()
            [(_ ((a ...) ...) ...)
             '(a ... ... ...)]))

        (t.is (my-append2 ((1 2) (3 4)) ((5) (6 7 8))) '(1 2 3 4 5 6 7 8))))
