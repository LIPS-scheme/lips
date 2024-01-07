(test.failing "continuations: base"
      (lambda (t)
        (define cont 0)
        (define result #f)

        (set! result (+ 2 (call/cc (lambda (cc)
                                     (set! cont cc)
                                     3))))
        (t.is result 5)
        (cont 4)
        (t.is result 6)))

(test.failing "continuations: double call/cc"
              (lambda (t)
                (define (repeat-string n item)
                  (apply string-append
                         (call/cc (lambda (return)
                                    (let ((next #f)
                                          (result '())
                                          (counter n))
                                      (call/cc (lambda (c)
                                                 (set! next c)))
                                      (set! counter (- counter 1))
                                      (set! result (cons item result))
                                      (if (zero? counter)
                                          (return result)
                                          (next)))))))
                (t.is (string-repeat 5 "x") "xxxxx")
                (t.is (string-repeat 2 "1") "11")))

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

;; example that found a bug in BiwaScheme
;; https://github.com/biwascheme/biwascheme/issues/257
(test.failing "continuations: saving/restoring environment"
      (lambda (t)
        (let ((result (call/cc (lambda (return)
                                 (let ((n 5)
                                       (result (list))
                                       (k #f))
                                   (set! result (append result (list (call/cc (lambda (return)
                                                                                (set! k return)
                                                                                "Hello")))))
                                   (when #t
                                     (if (zero? n)
                                         (return result))
                                     (set! n (- n 1))
                                     (k (string-append "Hello <" (number->string n) ">"))))))))

          (t.is result '("Hello <0>")))))
