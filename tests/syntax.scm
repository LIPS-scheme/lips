(test "syntax: hygiene"
      (lambda (t)
        (define result (let ((f (lambda (x) (+ x 1))))
                          (let-syntax ((f (syntax-rules ()
                                            ((_ x) x)))
                                       (g (syntax-rules ()
                                            ((_ x) (f x)))))
                            (list (f 1) (g 1)))))
         (t.is result '(1 2))))

(test "syntax: lambda"
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

(test "syntax: complex hygiene"
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

(test "syntax: let + return symbol"
      (lambda (t)
        (define result (let ((x 'outer))
                         (let-syntax ((m (syntax-rules () ((m) x))))
                           (let ((x 'inner))
                             (m)))))
        (t.is result 'outer)))


(test "syntax: quote expression"
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

(test "syntax: recursive or"
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

(test "syntax: rest (dot)"
      (lambda (t)
        (define result (let-syntax ((when (syntax-rules ()
                                            ((when test stmt1 . stmt2)
                                             (if test
                                                 (begin stmt1
                                                        . stmt2))))))
                         (define if #t)
                         (when if (set! if 'now) if)))
        (t.is result 'now)))

(test "syntax: double splice"
      (lambda (t)

        (define-syntax foo
          (syntax-rules ()
            ((foo (f1 ...) (f2 ...) . body-forms)
             '(f1 ... f2 ... . body-forms))))

        (t.is (foo (a b c d) (1 2 3 4) moe larry curly)
              '(a b c d 1 2 3 4 moe larry curly))))


(test "syntax: when syntax hygiene"
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

(test "syntax: function and macro"
      (lambda (t)

        (define even?
          (lambda (x)
            (or (= x 0) (odd? (- x 1)))))

        (define odd?
          (syntax-rules ()
            ((_ x) (not (even? x)))))

        (t.is (even? 10) #t)
        (t.is (even? 13) #f)))

(test "syntax: scope"
      (lambda (t)
        (let ()
          (define-syntax nil!
            (syntax-rules ()
              ((_ x)
               (set! x '()))))

          (let ((set! (lambda (x . rest) x))
                (x 10))
            (nil! x)
            (t.is x '())))))

(test "syntax: skip second item in list"
   (lambda (t)
     (define-syntax foo
       (syntax-rules () ((_ (a . (b . (c ...))) ...) '(foo (a c ... ) ...))))
     (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (1 3 4 5) (6 8 9 10)))))

(test "syntax: only cddr (list)"
   (lambda (t)

     (define-syntax foo
       (syntax-rules () ((_ (a b c ...) ...) '(foo (c ...) ...))))

     (t.is (foo) '(foo))
     (t.is (to.throw (foo 1)) #t)
     (t.is (to.throw (foo (1))) #t)
     (t.is (foo (1 2)) '(foo ()))
     (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (3 4 5) (8 9 10)))))

(test "syntax: only cddr (cons literals)"
   (lambda (t)

     (define-syntax foo
       (syntax-rules () ((_ (a . (b . (c ...))) ...) '(foo (c ...) ...))))

     (t.is (foo) '(foo))
     (t.is (to.throw (foo 1)) #t)
     (t.is (to.throw (foo (1))) #t)
     (t.is (foo (1 2)) '(foo ()))
     (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (3 4 5) (8 9 10)))))

(test "syntax: map on cddr"
   (lambda (t)

      (define-syntax foo
         (syntax-rules () ((_ x ...) (cons 'foo (map cddr '(x ...))))))

      (t.is (foo (1 2 3 4 5) (6 7 8 9 10)) '(foo (3 4 5) (8 9 10)))))

(test "syntax: extract 1st and 2nd items from list"
   (lambda (t)

      (define-syntax foo
         (syntax-rules () ((_ (a . (b . (c . ()))) ...) '(foo (a . c) ...))))

      (t.is (foo) '(foo))
      (t.is (foo (1 2 3)) '(foo (1 . 3)))
      (t.is (foo (1 2 3) (4 5 6)) '(foo (1 . 3) (4 . 6)))))

(test "syntax: extract 2nd elements from lists"
   (lambda (t)

      (define-syntax foo
         (syntax-rules () ((_ (a . (b . (c . ()))) ...) '(foo b ...))))

      (t.is (foo) '(foo))
      (t.is (to.throw (foo 1)) #t)
      (t.is (to.throw (foo (1))) #t)
      (t.is (to.throw (foo (1 2))) #t)
      (t.is (foo (1 2 3)) '(foo 2))
      (t.is (foo (1 2 3) (4 5 6)) '(foo 2 5))
      (t.is (foo (1 2 3) (4 5 6) (7 8 9)) '(foo 2 5 8))))

(test "syntax: should spread elements"
   (lambda (t)

      (define-syntax foo
         (syntax-rules ()
           ((_ (a . (b . (c . ()))) ...)
            '(foo a ... b ... c ...))))

      (t.is (foo) '(foo))
      (t.is (to.throw (foo 1)) #t)
      (t.is (to.throw (foo (1))) #t)
      (t.is (to.throw (foo (1 2))) #t)
      (t.is (foo (1 2 3) (4 5 6) (7 8 9)) '(foo 1 4 7 2 5 8 3 6 9))
      (t.is (foo (1 2 3)) '(foo 1 2 3))
      (t.is (foo (1 2 3) (4 5 6)) '(foo 1 4 2 5 3 6))))

(test "syntax: list quine"
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

(test "syntax: cons 1st and 2nd in lists"
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

(test "syntax: zip transformation"
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

(test "syntax: merge lists"
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

(test "syntax: identifiers"
      (lambda (t)
        (define-syntax let+
          (syntax-rules (==>)
            ((_ ((a ==> b) ...) . body)
             (let ((a b) ...) . body))))

        (t.is (let+ ((a ==> 1)
                     (b ==> 2))
                    (+ a b))
              3)))

(test "syntax: basic ellipsis (srfi-46)"
      (lambda (t)

        (define-syntax funcall
          (syntax-rules ::: ()
             ((_ name args :::) (name args :::))))

        (t.is (funcall list 1 2 3) '(1 2 3))))

(test "syntax: macro define function"
      (lambda (t)

        (define-syntax def
          (syntax-rules (==>)
            ((_ name ==> body ...)
             (define name (lambda body ...)))))

        (def square ==> (x) (* x x))
        (t.is (square 10) 100)))

(test "syntax: macro define list of functions"
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

(test "syntax: nested syntax-rules (srfi-46)"
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

(test "syntax: nested syntax-rules gensyms (srfi-46)"
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

(test "syntax: tail of ellipsis (srfi-46)"
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

(test "syntax: rec macro (srfi-31)"
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

(test "syntax: join macros"
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

(test "syntax: double ellipsis (SRFI-149)"
      (lambda (t)

        (define result (let-syntax
                           ((my-append
                             (syntax-rules ()
                               ((my-append (a ...) ...) '(a ... ...)))))
                         (my-append (1 2 3) (4 5 6))))

        (t.is result '(1 2 3 4 5 6))))

(test "syntax: nested macro with escape ellipsis"
      (lambda (t)
        (define-syntax define-for
          (syntax-rules ()
            ((_ symbol)
             (define-syntax symbol
               (syntax-rules ()
                 ((_ (var start end) body (... ...))
                  (let loop ((var start))
                    (if (<= var end)
                        (begin
                          body (... ...)
                          (loop (+ var 1)))))))))))

        (define-for loop)

        (let ((result (vector)))
          (loop (i 1 10)
                (result.push i))
          (t.is result #(1 2 3 4 5 6 7 8 9 10)))))

(test "syntax: triple elispsis (Gauche example)"
      (lambda (t)
        (define-syntax my-append
          (syntax-rules ()
            [(_ ((a ...) ...) ...)
             '(a ... ... ...)]))

        (t.is (my-append ((1 2) (3 4)) ((5) (6 7 8))) '(1 2 3 4 5 6 7 8))))

(test "syntax: my-let"
      (lambda (t)
        (define-syntax my-let
          (syntax-rules ()
            [(_ ((var init) ...) body ...)
             ((lambda (var ...) body ...) init ...)]))

        (t.is (my-let ((x 10) (y 20)) (+ x y)) 30)))

(test.failing "syntax: lifted ellipsis (SRFI-149)"
      (lambda (t)
        (define result
          (let-syntax
              ((foo (syntax-rules ()
                      ((foo (a b ...) ...) '(((a b) ...) ...)))))
            (foo (bar 1 2) (baz 3 4))))

        (t.is result '(((bar 1) (bar 2)) ((baz 3) (baz 4))))))


(test "syntax: R6RS do macro"
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

;; foo foo ... should match single element foo ... should match ()
(test "syntax: R6RS unless & when macros"
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
         (t.is (when (< 3 2) 'foo) #void) ;; unspecified

         (t.is (unless (> 3 2) 'less) #void) ;; unspecified

         (t.is (unless (< 3 2) 'foo) 'foo)))

;; guile example
(test "syntax: literal atoms"
       (lambda (t)
          (define-syntax define-matcher-macro
            (syntax-rules ()
              ((_ name lit)
               (define-syntax name
                 (syntax-rules ()
                  ((_ lit) #t)
                  ((_ else) #f))))))

             (define-matcher-macro is-literal-foo? "foo")

             (t.is (is-literal-foo? "foo") #t)
             (t.is (is-literal-foo? "bar") #f)
             (let ((foo "foo"))
                (t.is (is-literal-foo? foo) #f))))

(test "syntax: my-or hygiene"
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


(test "syntax: recursive do"
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


(test "syntax: should define nested syntax-rules"
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

(test "syntax: recursive call"
      (lambda (t)

        (define-syntax L
          (syntax-rules ()
            ((_) '())
            ((_ a b ...) (cons a (_ b ...)))))

        (t.is (L 1 2 3) '(1 2 3))))

(test.failing "syntax: should return list with ellipsis"
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


(test "syntax: should handle identifiers"
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


(test "syntax: should define let*"
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

        (t.is (foo 1 ++ 2) '(1 1 1 2))))

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

(test.failing "syntax: free variables"
      (lambda (t)
        (define-syntax def (syntax-rules ()
                             ((_ foo bar)
                              (begin
                                (define foo bar)
                                hello))))

        (t.is (def hello 10) 10)
        (def hello 10)
        (t.is hello 10)))

(test "syntax: should pass body from macro to function"
      (lambda (t)

        (define-syntax foo
          (syntax-rules ()
            ((_ . bar) (baz . bar))))

        (define (baz . args) args)

        (t.is (foo 1 2 3) '(1 2 3))))

(test "syntax: should find last item in list"
      (lambda (t)

        (define-syntax last
          (syntax-rules ()
            ((_ ()) ())
            ((_ (x)) x)
            ((_ (x ... y)) y)))

        (t.is (last (1 2 3 4 5 6)) 6)
        (t.is (last (1)) 1)
        (t.is (last ()) ())
        (t.is (to.throw (last)) true)))

(test "syntax: should find last item in argument list"
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


(test "syntax: should skip cons with identifier"
      (lambda (t)
        (define-syntax foo
          (syntax-rules (<>)
            ((_ <> . b) ())
            ((_ a . b) (cons a b))))

        (t.is (foo 1) '(1))
        (t.is (foo 1 . 2) '(1 . 2))))

(test "syntax: should define nested syntax with variable from outside as identifier"
      (lambda (t)

        (define-syntax foo (syntax-rules ()
                     ((_ bar)
                      (let ()
                        (define-syntax baz
                          (syntax-rules (bar)
                            ((_ x) ())))
                        (baz bar)))))

        (t.is (foo 10) ())))

(test "syntax: should expand in nested syntax into variable from parent syntax"
      (lambda (t)
        (define-syntax foo (syntax-rules ()
                     ((_ bar quux)
                      (let ()
                        (define-syntax baz
                          (syntax-rules (bar)
                            ((_ x) (list quux))))
                        (baz bar)))))

        (t.is (foo 1 "hello") '("hello"))))

(test "syntax: should expand nested macro with ellipsis as identifier from parent"
      (lambda (t)

        (define-syntax foo
          (syntax-rules (ellipsis)
            ((_)
             (let ()
               (define-syntax foo
                 (syntax-rules ellipsis ()
                   ((_ x ellipsis)
                    (list x ellipsis))))
               (foo 1 2 3)))))

        (t.is (foo) '(1 2 3))

        ;; recursive case
        (define-syntax foo
          (syntax-rules (ellipsis)
            ((_)
             (let ()
               (define-syntax foo
                 (syntax-rules ellipsis ()
                   ((_) ())
                   ((_ x) (list x))
                   ((_ x ellipsis)
                    (list (foo x) ellipsis))))
               (foo 1 2 3)))))

        (t.is (foo) '((1) (2) (3)))))

(test "syntax: should ignore ellipsis in middle for 2 elements"
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
        ;; example from book Sketchy Scheme by Nils M Holm
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

(test "syntax: R7RS multiple ellipsis extensions"
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

(test "syntax: method on pattern symbol"
      (lambda (t)
        (define-syntax let*-values
            (syntax-rules ()
               ((_ ((bind values)) body ...)
                (apply (lambda bind
                         body ...)
                       (vector->list (values.valueOf))))))

        (t.is (let*-values (((a b c) (values 1 2 3)))
                           (+ a b c))
              6)))


;; ref: https://stackoverflow.com/a/64659565/387194
(test "syntax: alist"
      (lambda (t)
        (define-syntax alist
          (syntax-rules ()
            ((_) ())
            ((_ a b) (list (cons a b)))
            ((_ x y z ...)
             (cons (cons x y) (alist z ...)))))

        (t.is (alist "foo" 1 "bar" 2 "baz" 3)
              '(("foo" . 1) ("bar" . 2) ("baz" . 3)))))

(test "syntax: alist + rest"
      (lambda (t)
        (define-syntax alist
          (syntax-rules ()
            ((_) ())
            ((_ a b) (list (cons a b)))
            ((_ x y . rest)
             (cons (cons x y) (alist . rest)))))

        (t.is (alist "foo" 1 "bar" 2 "baz" 3)
              '(("foo" . 1) ("bar" . 2) ("baz" . 3)))))

(test.failing "syntax: nested _"
       (lambda (t)
         (define-syntax foo
           (syntax-rules ()
             ((_)
              (let ()
                (define-syntax %foo
                  (syntax-rules (foo bar)
                    ((_ (foo))
                     "foo")
                    ((_) "bar")
                    ((_ x)
                     'x)))
                (list (%foo (foo))
                      (%foo (10))
                      (%foo bar)
                      (%foo))))))

         (t.is (foo) '("foo" (10) bar "bar"))))

(test.failing "syntax: nesting, renaming and scope"
       (lambda (t)
         (let ((result 10))
           (define-syntax foo
             (syntax-rules ()
               ((_)
                (let ()
                  (define-syntax %foo
                    (syntax-rules (foo bar)
                      ((__ (foo))
                       (set! result '(foo)))
                      ((__ x)
                       (set! result 'x))))
                  (%foo (foo))))))
           (foo)
           (t.is result '(foo)))))

(test "syntax: nested syntax-rules scope conflict"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ bar)
             (let ()
               (define-syntax %foo
                 (syntax-rules (bar)
                   ((_ bar x)
                    (list "foo" x))))
               (%foo bar 10)))))

        (t.is (foo x) '("foo" 10))

        (define-syntax foo
          (syntax-rules ()
            ((_ x)
             (let ()
               (define-syntax %foo
                 (syntax-rules (bar)
                   ((_ (bar) x)
                    (list "foo" x))))
               (%foo (bar) 10)))))

        (t.is (foo 10) '("foo" 10))))

(test "syntax: should throw error on missing ellipsis symbol"
      (lambda (t)
        (t.is
         (to.throw
          (define-syntax foo
            (syntax-rules (:c)
              ((_ x ...)
               (letrec-syntax ((bar (syntax-rules <:::> (:c)
                                                  ((_ x)
                                                   (print x))
                                                  ((_ a b <:::>)
                                                   (begin
                                                     (display a)
                                                     (display " ")
                                                     (bar b <:::>))))))
                 (bar x ...)))))
          (foo 1 2 3))
         true)))

(test "syntax: should create macro with dot notation as pattern variable"
      (lambda (t)
        (let* ((input #(1 0.1 2 3 10e-1))
               (fn (lambda (x) (+ x 1)))
               (expect (input.map fn)))
          (let-syntax ((foo (syntax-rules ()
                              ((_ x)
                               (x.map fn)))))
            (t.is (foo input) expect)))))

(test "syntax: should work with dot notation in lambda inside syntax-rules"
      (lambda (t)
        (let* ((input '(1 0.1 2 3 10e-1))
               (expect (map (lambda (x) (x.isFloat)) input)))
          (let-syntax ((foo (syntax-rules ()
                              ((_)
                               (lambda (num) (num.isFloat))))))
            (let ((is-float (foo)))
              (t.is (map is-float input) expect))))))

(test "syntax: refsh macro"
      (lambda (t)
        (define-syntax fresh
          (syntax-rules ()
            ((_ (sym ...) expr exprs ...)
             (let ((sym (if #f #f)) ...)
               expr exprs ...))))

        (t.is (fresh (a b c) (list a b c))
              (list #void #void #void))))

(test "syntax: macro from Petrofsky"
      (lambda (t)
        ;; https://groups.google.com/g/comp.lang.scheme/c/FB1HgUx5d2s
        (t.is (letrec-syntax ((foo (syntax-rules (foo) ((_ foo) #t) ((_ x) #f))))
                (foo foo))
              #t)
        (define foo #null)
        (t.is (letrec-syntax ((foo (syntax-rules (foo) ((_ foo) #t) ((_ x) #f))))
                (foo foo))
              #t)))

(test.failing "syntax: let-syntax and set! of definition"
      (lambda (t)
        ;; https://github.com/jcubic/lips/issues/172
        (define-syntax g
          (syntax-rules ()
            ((g 2) -3)))

        (t.is (let-syntax ((f (syntax-rules ()
                          ((f 1) (g 2)))))
                (set! g (lambda (x) -1000))
                (f 1))
              -3)))

(test "syntax: syntax-rules -> syntax-rules"
      (lambda (t)
        ;; source: https://srfi.schemers.org/srfi-147/srfi-147.html
        (define-syntax syntax-rules*
          (syntax-rules ()
            ((syntax-rules* (literal ...) (pattern . templates) ...)
             (syntax-rules (literal ...) (pattern (begin . templates)) ...))
            ((syntax-rules* ellipsis (literal ...) (pattern . templates) ...)
             (syntax-rules ellipsis (literal ...) (pattern (begin . templates)) ...))))

        (t.is (let-syntax
                  ((foo
                    (syntax-rules* ()
                                   ((foo a b)
                                    (define a 1)
                                    (define b 2)))))
                (foo x y)
                (list x y))
              '(1 2))))

(test.failing "syntax: syntax-parameterize SRFI 139"
      (lambda (t)

         (define-syntax-parameter it
           (syntax-rules ()
             ((_ . _)
              (syntax-error "it used outside of a aif"))))

        (define-syntax aif
          (syntax-rules (aux)
            ((_ aux test x y ...)
             (let ((value test))
               (syntax-parameterize
                ((it (syntax-rules ()
                       ((it) value))))
                (if value
                    x
                    y ...))))
            ((_ test true)
             (aif aux test true))
            ((_ test true false)
             (aif aux test true false))))

        (let ((alist '((foo . 10) (bar . 20))))
          (it.s (aif (assoc 'foo alist) (cdr (it)))
                10)
          (t.is (aif (assoc 'x alist) (cdr (it)))
                (if #f #f)))))

(test "syntax: should throw a proper error on not matched syntax"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ var1 ... var2)
             (begin
               (print var1)
               ...
               (print var2)))))

        (t.is (Boolean (--> (try (foo) (catch (e) e.message))
                            (match #/^syntax-rules: no matching syntax in macro/)))
              #t)))

(test "syntax: should match pattern (_ () var1 ... var2) #244"
      (lambda (t)
        (t.plan 1)
        (define-syntax foo
          (syntax-rules ()
            ((_ () var1 ... var2)
             (begin
               (string-append var1 " ")
               ...
               var2))))

        (foo () "a" "b")
        (foo () "x")
        (t.is #t #t)))

(test "syntax: recursive use of free variable hygiene #288"
      (lambda (t)
        (define-syntax call/mv
          (syntax-rules ()
            ((call/mv consumer producer1 ...)
             (letrec-syntax
                 ((aux (syntax-rules ::: ()
                         ((aux %consumer () ((%producer1 args1) :::))
                          (let-values (((proc) %consumer)
                                       (args1 %producer1) :::)
                            (apply proc (append args1 :::))))
                         ((aux %consumer (%producer1 producer2 :::) (temp :::))
                          (aux %consumer (producer2 :::) (temp ::: (%producer1 args1)))))))
               (aux consumer (producer1 ...) ())))))


        (t.is (call/mv string (values #\a #\b) (values #\c #\d)) "abcd")))

(test "syntax: SRFI-147"
      (lambda (t)
        (define-syntax syntax-rules*
          (syntax-rules ()
            ((syntax-rules* (literal ...) (pattern . templates) ...)
             (syntax-rules (literal ...) (pattern (begin . templates)) ...))
            ((syntax-rules* ellipsis (literal ...) (pattern . templates) ...)
             (syntax-rules ellipsis (literal ...) (pattern (begin . templates)) ...))))

        (let-syntax ((foo
                      (syntax-rules* ()
                        ((foo a b)
                         (define a 1)
                         (define b 2)))))
          (foo x y)
          (t.is (list x y) '(1 2)))))

(test "syntax: ellipsis + improper list"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ (foo bar ... . baz))
             '(foo baz))))

        (t.is (foo (a . b)) '(a b))))

(test "syntax: recursive hygiene with same symbol"
      (lambda (t)
        (define-syntax foo
          (syntax-rules (aux)
            ((_ (arg more ...))
             (foo aux (arg more ...) ()))
            ((_ aux () ((operand1 arg1) ...))
             (let ((arg1 operand1) ...)
               (list arg1 ...)))
            ((_ aux (operand1 operand2 ...) (temp ...))
             (foo aux (operand2 ...) (temp ... (operand1 arg1))))))

        (t.is (foo (10 20)) '(10 20))))

(test "syntax: recursive hygiene with nested syntax-rules"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ (arg more ...))
             (letrec-syntax ((aux (syntax-rules ::: ()
                                    ((aux () ((operand1 arg1) :::))
                                     (let ((arg1 operand1) :::)
                                       (list arg1 :::)))
                                    ((aux (operand1 operand2 :::) (temp :::))
                                     (aux (operand2 :::) (temp ::: (operand1 arg1)))))))
               (aux (arg more ...) ())))))

        (t.is (foo (10 20)) '(10 20))))

(test "syntax: cons spread"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ (var1 ... . var*))
             '(var1 ... var*))))

        (t.is (foo (x . y)) '(x y))))

(test "syntax: symbol after spread with ()"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ x () ((a b) ...) z)
             '(let ()
                (x (a b) ... z)
                (foo b ... args)))))

        (t.is (foo (print x) () () (display x))
              '(let () ((print x) (display x)) (foo args)))

        (define-syntax foo
          (syntax-rules ()
            ((_ x () ((a b) ...) z)
             '(let ()
                (x (a b) ... z)
                (foo (a ...) (b ...) args)))))

        (t.is (foo (print x) () () (display x))
              '(let () ((print x) (display x)) (foo () () args)))))

(test "syntax: spread tail"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ ((p ...) . body))
             '(apply (lambda (p ...) . body)
                     args))
            ((_ ((p ... . tail) . body))
             '(apply (lambda (p ... . tail) . body)
                     args))))

        (t.is (foo ((lis transducer . transducers) (display x)))
              '(apply (lambda (lis transducer . transducers) (display x)) args))))

(test "syntax: multiple values after ellipsis"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ (a ... b c) d ...)
             (list a ... b c d ...))))

        (t.is (foo (1 2 3 'x 'y) "foo" "bar" "baz")
              '(1 2 3 x y "foo" "bar" "baz"))))

;; ref: https://stackoverflow.com/q/37644555/387194
(test "syntax: identifier with variable"
      (lambda (t)
        (define-syntax hello
          (syntax-rules (in)
            ((_ name in world) (format "Hello ~a in ~a" name world))
            ((_ in name) (format "Hello ~a in here" name))))

        (define in "inside")
        (t.is (hello "me" in in)
              "Hello me in inside")))

;; ref: https://practical-scheme.net/gauche/man/gauche-refe/Hygienic-macros.html#Syntax_002drules-macro-transformer
(test "syntax: let shadow identifier (1)"
      (lambda (t)
        (define-syntax if+
          (syntax-rules (then else)
            ((_ test then expr1 else expr2) (if test expr1 expr2))))

        (define else #f)
        (let ((x 10))
          (t.is (if+ (even? x) then (/ x 2) else (/ (+ x 1) 2))
                5))

        (t.is (to.throw (let ((else #f) (x 10))
                          (if+ (even? x) then (/ x 2) else (/ (+ x 1) 2))))
              #t)))

(test "syntax: let shadow identifier (2)"
      (lambda (t)
        (define else #f)
        (define-syntax if+
          (syntax-rules (then else)
            ((_ test then expr1 else expr2) (if test expr1 expr2))))

        (let ((x 10))
          (t.is (if+ (even? x) then (/ x 2) else (/ (+ x 1) 2))
                5))

        (t.is (to.throw (let ((else #f) (x 10))
                          (if+ (even? x) then (/ x 2) else (/ (+ x 1) 2))))
              #t)))

(test "syntax: nested spread + leftover"
      (lambda (t)
        (define-syntax quux
          (syntax-rules ()
            ((_ (x ... a b) ...)
             '((x ... b) ...))))

        (t.is (quux (1 2 3 4) (5 6 7 8) (9 10 11 12))
              '((1 2 4) (5 6 8) (9 10 12)))))

(test "syntax: list as last element after ellipsis"
      (lambda (t)
        (define-syntax quux
          (syntax-rules ()
            ((_ (x ... (a ...)) ...)
             '((a ...) ...))))

        (t.is (quux (1 2 3 (1 2 3))
                     (5 6 7 (8))
                     (9 10 11 (12)))
              '((1 2 3) (8) (12)))

        (define-syntax quux
          (syntax-rules ()
            ((_ (x ... (a ...)) ...)
             '(a ... ...))))

        (t.is (quux (1 2 3 (1 2 3))
                    (5 6 7 (8))
                    (9 10 11 (12)))
              '(1 2 3 8 12))))

(test "syntax: helper macro pattern"
      (lambda (t)
        (define-syntax foo
          (syntax-rules (aux)
            ((_ (x ... (a ...) (b ...)) ...)
             (foo aux (a ... b ...) ...))
            ((_ aux (a ...) (b ...))
             '(a ... b ...))))

        (t.is (foo (1 2 (a b) (c d)) (3 4 (e f) (g h)))
              '(a b c d e f g h))))

(test "syntax: vectors as symbols"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ sym ...)
             (list sym ...))))

        (t.is (foo #(1 2) #(3 4)) '(#(1 2) #(3 4)))))

(test "syntax: make-vector"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ #(x ...) ...)
             (vector x ... ...))))

        (t.is (foo #(1 2) #(3 4))
              #(1 2 3 4))))

(test "syntax: vector and symbol + rest"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ #(a b ...) ...)
             (vector (list a b ...) ...))))

        (t.is (foo #(1 2 3) #(4 5 6))
              #((1 2 3) (4 5 6)))))

(test "syntax: vector 3 ellipsis"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((_ #(#(a b ...) ...) ...)
             (vector '(a b ...) ... ...))))

          (t.is (foo #(#(1 2 3) #(4 5 6)) #(#(1 2)))
                #((1 2 3) (4 5 6) (1 2)))))

(test "syntax: vector ellipsis + symbols after"
      (lambda (t)
        (define-syntax quux
          (syntax-rules ()
            ((_ #(a b ... z) ...)
             (vector 'z ...))))

        (t.is (quux #(1 2 3) #(4 5 6) #(7 8 9))
              #(3 6 9))

        (define-syntax quux
          (syntax-rules ()
            ((_ #(x ... a b) ...)
             (vector '(a b) ...))))

        (t.is (quux #(1 2 3 4) #(5 6 7 8) #(9 10 11 12))
              #((3 4) (7 8) (11 12)))

        (define-syntax quux
          (syntax-rules ()
            ((_ #(x ... a b) ...)
             (vector '(x ...) ...))))

        (t.is (quux #(1 2 3 4) #(5 6 7 8) #(9 10 11 12))
              #((1 2) (5 6) (9 10)))))

(test "syntax: simple vector spread"
      (lambda (t)
        (define-syntax quux
          (syntax-rules ()
            ((_ #(x ... a b) ...)
             #(b ...))))

        (t.is (quux #(1 2 3 4) #(5 6 7 8) #(9 10 11 12))
              #(4 8 12))))

(test "syntax: recursive flatten"
      (lambda (t)
        (define-syntax flatten
          (syntax-rules (aux reverse)
            ((_ xs)
             (flatten aux xs ()))
            ((_ aux ((xs ...) ys ...) (result ...))
             (flatten aux (xs ... ys ...) (result ...)))
            ((_ aux (x xs ...) (result ...))
             (flatten aux (xs ...) (x result ...)))
            ((_ aux () (result ...))
             (flatten reverse (result ...) ()))
            ((_ reverse () (result ...))
             '(result ...))
            ((_ reverse (x xs ...) (result ...))
             (flatten reverse (xs ...) (x result ...)))))

        (t.is (flatten ((1 2 (a b) (c d)) (3 4 (e f) (g h))))
              '(1 2 a b c d 3 4 e f g h))))

(test "syntax: let-slim"
      (lambda (t)
        ;; ref https://stackoverflow.com/a/56419718/387194
        (define-syntax let-slim
          (syntax-rules (pair)
            ((_ pair bindings () body)
             (let bindings . body))
            ((_ pair (acc ...) (k v . rest) body)
             (let-slim pair (acc ... (k v)) rest body))
            ((_ (elements ...) . body)
             (let-slim pair () (elements ...) body))))

        (t.is (let-slim (x 10 y 20)
                        (+ x y))
              30)))

(test "syntax: undswap"
      (lambda (t)
        ;; ref: https://stackoverflow.com/a/58965190/387194
        (define-syntax undswap
          (syntax-rules (_)
            ((undswap val (e ...))
             ((undswap val e) ...))
            ((undswap val _) val)
            ((undswap val e) e)))

        (t.is (undswap 3 (if _ (+ 3 _ )))
              6)))

(test "syntax: alist into code"
      (lambda (t)
        (define-syntax alist
          (syntax-rules ()
            ((_)
             '())
            ((_ key value . rest)
             (cons (cons key value) (alist . rest)))))

        (t.is (alist 'foo 10 'bar 20 'baz 30)
              '((foo . 10) (bar . 20) (baz . 30)))))

(test "syntax: alist literal"
      (lambda (t)
        ;; ref: https://stackoverflow.com/a/64672095/387194
        (define-syntax alist
          (syntax-rules (alist-builder)
            ((_ alist-builder () (results ...))
             '(results ...))
            ((_ alist-builder (a) . rest)
             (raise 'bad-alist))
            ((_ alist-builder (a b rest ...) (results ...))
             (alist alist-builder (rest ...) (results ... (a . b))))
            ((_ a ...) (alist alist-builder (a ...) ()))))

        (t.is (alist foo 10 bar 20 baz 30)
              '((foo . 10) (bar . 20) (baz . 30)))))

(test "syntax: nested syntax rules (SRFI-239 case)"
      (lambda (t)
        (define-syntax foo
          (syntax-rules ()
            ((foo expr clauses ...)
             (let-syntax ((clause
                           (syntax-rules ::: (_ pair null doted matched)
                             ((clause obj pair n d ((_ . _) body1 ::: body2) remaining :::)
                              (if (pair? obj)
                                  (begin body1 ::: body2))))))
               (let ((obj expr))
                 (clause obj pair null doted clauses ...))))))

        (t.is (foo '(1 2) ((_ . _) 'pair)) 'pair)))
