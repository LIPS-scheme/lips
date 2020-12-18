(test "core: it should set!/set-obj! with this and prototype"
      (lambda (t)
        (let ()
          (define foo (lambda (x) (set! this.x x)))
          (define bar (new foo 10))
          (set! foo.prototype.square (lambda (x) (* x x)))
          (set! foo.prototype.sum (lambda (x) (+ this.x x)))
          (t.is (bar.square 10) 100)
          (t.is (bar.sum 5) 15))
        (let ()
          (define foo (lambda (x) (set-obj! this "x" x)))
          (define bar (new foo 10))
          (set-obj! foo.prototype 'square (lambda (x) (* x x)))
          (set-obj! foo.prototype 'sum (lambda (x) (+ this.x x)))
          (t.is (bar.square 10) 100)
          (t.is (bar.sum 5) 15))))

(test "core: let/letrect/let*"
      (lambda (t)
        ;; tests based on book Sketchy Scheme by Nils M Holm
        (t.is (to.throw (let ((down (lambda (x)
                                      (if (zero? x)
                                          0
                                          (down (- x 1))))))
                          (down 5)))
              true)

         (t.is (letrec ((down (lambda (x)
                                (if (zero? x)
                                    0
                                    (down (- x 1))))))
                 (down 5))
               0)

         (let ((a '(outer-a))
               (b '(outer-b)))
           (let ((a '(0))
                 (b (cons 1 a))
                 (c (cons 2 b)))
             (t.is c '(2 outer-b))))

         (let ((a '(outer-a))
               (b '(outer-b)))
           (let* ((a '(0))
                  (b (cons 1 a))
                  (c (cons 2 b)))
             (t.is c '(2 1 0))))

         (t.is (letrec ((a 10)
                        (b 20))
                 (+ a b))
               30)

         (t.is (letrec ((a 10)
                        (b a))
                  (+ a b))
               20)

         (t.is (to.throw (let ((a 10)
                               (b a))
                           (+ a b)))
               true)))

(test "core: it should throw on set! with undefined variable"
      (lambda (t)
        (t.is (to.throw (set! foo.bar 10)) true)
        (t.is (to.throw (set! foo 10)) true)))

(test "core: it should set object"
      (lambda (t)
        (let ((x (object :foo "jo")))
          (set! x.bar "hey")
          (t.is (string-append (--> x.bar (toUpperCase))
                               " "
                               (x.foo.toUpperCase))
                "HEY JO"))))

(test "core: it should throw when set object literal"
      (lambda (t)
        (let ((x &(:foo "jo")))
          (t.is (to.throw (set! x.foo "hey")) true)
          (t.is (to.throw (set! x.bar "hey")) true))))


(test "core: it should throw when set vector literal"
      (lambda (t)
        (let ((x #(0 1)))
          (t.is (to.throw (set! x.0 2)) true)
          (t.is (to.throw (x.push 3)) true))))

(test "core: timing test"
      (lambda (t)
        (--> t (is (function? (.. Date.now)) true))
        (define start (--> Date (now)))
        (wait 100 (--> t (is (>= (- (--> Date (now)) start) 100) true)))))

(test "core: values"
      (lambda (t)
        (t.is (call-with-values * -) -1)
        (t.is (call-with-values (lambda () (values 4 5))
                (lambda (a b) b)) 5)
        (t.is (call-with-values (lambda () (values 4 5)) +) 9)))

(test "core: symbols"
      (lambda (t)
        (t.is '|foo\x20;bar| (string->symbol "foo bar"))
        (t.is '|\n| (string->symbol "\n"))
        (t.is '|\t\t| (string->symbol "\t\t"))
        (t.is '|\r| (string->symbol "\r"))
        (t.is '|\s| 's)
        (t.is '|\x3BB;| 'λ)
        (t.is '|\x9;\x9;| '|\t\t|)))

(test "core: if"
      (lambda (t)
        (t.is (if (newline) 1 2) 1)
        (t.is (if 0 1 2) 1)
        (t.is (if null 1 2) 1)
        (t.is (if () 1 2) 1)
        (t.is (if #f 1 2) 2)))

(test "core: and/or"
      (lambda (t)
        (t.is (and) #t)
        (t.is (or) #f)
        ;; undefined and null should be true values
        ;; according to spec #f should be the only fasly value
        (t.is (and 1 undefined) undefined)
        (t.is (and 1 null) null)
        (t.is (or (begin) 1) undefined)
        (t.is (or null 1) null)))

(test_ "core: do macro"
      (lambda (t)
        (t.is (do ((i 0) (j 10 (- j 1))) (i j)) 10)
        (t.is (do ((i 0) (j 10 (- j 1))) (null j)) 10)
        (t.is (do ((i 0) (j 10 (- j 1))) (undefined j)) 10)
        (t.is (do ((i 0) (j 10 (- j 1))) ((zero? j) 10)) 10)))

(test "core: eq?/eqv?"
      (lambda (t)
        ;; TODO
        ;;eq? bool nil symbol

        ;;eqv? number char + eq?
        (t.is true true)))

(test "core: scheme signature"
      (lambda (t)
        ;; we should know about changing of signature
        (load "./examples/scheme-detect.scm")

        (t.is (detect:name) 'lips)))

(test "core: input-string-port"
      (lambda (t)

        (let ((port (open-input-string "`(```,,,,@(list 1 2)) 10 /foo bar/")))
          (t.is (read port) '(quasiquote ((quasiquote (quasiquote (quasiquote (unquote (unquote (unquote (unquote-splicing (list 1 2)))))))))))
          (t.is (read port) 10)
          (t.is (read port) /foo bar/)
          (t.is (eof-object? (read port)) true))))

(test "core: it should throw exception why calling with improper list"
      (lambda (t)
        (t.is (to.throw (let ((x '(1 2))) (+ 1 . x))) true)))

(test "core: it should throw exception why applying function to improper list"
      (lambda (t)
        (t.is (to.throw (let ((x '(1 2 . 3))) (apply + x))) true)))


(test "core: async for-each"
      (lambda (t)
          (define (delay x)
            (new Promise (lambda (r)
                           (setTimeout r x))))
          (let* ((result (vector))
                 (push (lambda (x) (--> result (push x))))
                 (count 4)
                 (time 100)
                 (numbers (map (curry + 1) (range count)))
                 (start (Date.now)))
            (push 0)
            (for-each (lambda (x)
                        (delay time)
                        (--> result (push x)))
                      numbers)
            (push (+ 1 count))
            (let ((end (Date.now)))
              (t.is (>= (- end start) (* (+ count 1) time)) true))
            (t.is result (--> #(0) (concat (list->vector numbers) (vector (+ count 1))))))))

(test "core: access this in method"
      (lambda (t)
        (let* ((x (object :foo (lambda () this.bar) :bar 10)))
          (t.is (x.foo) 10))))

(test "core: access env in called function inside method"
      (lambda (t)
        (let* ((result (vector))
               (x (object :foo (lambda ()
                                 (for-each (lambda (x)
                                             (--> result (push x)))
                                           '(1 2 3))))))
          (x.foo)
          (t.is result #(1 2 3)))))

(test "core: access this in closure returned from method"
      (lambda (t)
        (let* ((x (object :foo (lambda ()
                         (lambda (x)
                           (+ x this.bar)))
                          :bar 1))
               (fn (x.foo)))
          (t.is (fn 2) 3))))

