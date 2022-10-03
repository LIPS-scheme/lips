(test "std: case-lambda"
      (lambda (t)

        ;; example from R7RS without do macro
        (define range
          (case-lambda
           ((e) (range 0 e))
           ((b e) (let iter ((result ()) (i (- e 1)))
                    (if (< i b)
                        result
                        (iter (cons i result) (- i 1)))))))

        (t.is (range 3) '(0 1 2))
        (t.is (range 3 5) '(3 4))))

(test "std: string-map"
      (lambda (t)
        ;; example from R7RS spec
        (define result (string-map
                        (lambda (c k)
                          ((if (eqv? k #\u) char-upcase char-downcase)
                           c))
                        "studlycaps xxx"
                        "ululululul"))

        (t.is result "StUdLyCaPs")))

(test "std: vector-map"
      (lambda (t)

        ;; examples from R7RS spec
        (t.is (vector-map cadr '#((a b) (d e) (g h)))
              '#(b e h))

        (t.is (vector-map + '#(1 2 3) '#(4 5 6 7))
              '#(5 7 9))))

(test "std: some"
      (lambda (t)
        (t.is (some + nil) #f)
        (t.is (some odd? (list 1 2 3)) #t)
        (t.is (some odd? (list 2 4 6)) #f)))

(test "std: fold"
      (lambda (t)
        (t.is (fold * 1 (cdr (range 10))) 362880)))

(test "std: pluck"
      (lambda (t)
        (let ((name (pluck '__name__)))
          (t.is (name 'foo) "foo"))
        (let ((name (pluck "__name__")))
          (t.is (name 'foo) "foo"))
        (let ((none (pluck)))
          (t.is (none 'foo) nil))
        (let ((xy (pluck 'x 'y)))
          (t.is (xy &(:x 10 :y 20 :z 30)) &(:x 10 :y 20)))))

(test "std: predicates"
      (lambda (t)
        (t.is (regex? #/foo/) #t)
        (t.is (boolean? nil) #f)
        (t.is (boolean? null) #f)
        (t.is (boolean? undefined) #f)
        (t.is (boolean? #t) #t)
        (t.is (boolean? #f) #t)))

(test "std: find"
      (lambda (t)
        (t.is (find odd? (list 1 2 3)) 1)
        (t.is (find #/^[0-9]+$/ (list "foo" "bar" "10")) "10")
        (t.is (to.throw (find "xxx" (list 1 2 3))) true)
        (t.is (find odd? (list 0 2 4 3)) 3)
        (t.is (find odd? (list 0 2 4 6)) nil)))

(test "std: typecheck"
      (lambda (t)
        (t.is (to.throw (typecheck "test" 10 (list "string"))) true)
        (t.is (try (typecheck "test" 10 (list "string") 0) (catch (e) e.message))
              "Expecting string, got number in expression `test` (argument 0)")
        (t.is (try (typecheck "test" 10 (list "string" "character") 0) (catch (e) e.message))
              "Expecting string or character, got number in expression `test` (argument 0)")))

(test "std: let-values and let*-values"
      (lambda (t)

        (let ((a 10) (b 20) (c 30))
          (let*-values (((a b c) (values 1 2 3)) ((x y z) (values a b c)))
            (t.is (+ a b c x y z) 12)))

        (let ((a 10) (b 20) (c 30))
          (let-values (((a b c) (values 1 2 3)) ((x y z) (values a b c)))
            (t.is (+ x y z) 60)
            (t.is (+ a b c) 6)))))

(test "std: should render SXML string"
      (lambda (t)
        (define preact (require "preact"))
        (define h preact.h)
        (define jsx->string (require "preact-render-to-string"))

        (define-class Button1 preact.Component
          (render (lambda (self props state)
                    (sxml (button (@ (id "btn1"))
                                  props.label)))))

        (define (Button2 props)
          (sxml (button (@ (id "btn2") (onClick (lambda () (alert "hello world")))) props.label)))

        (t.snapshot  (jsx->string (sxml (div (@ (data-foo "hello")
                                                (id "foo"))
                                             (Button1 (@ (label "me")))
                                             (Button2 (@ (label "me")))))))))


(test "std: fold/curry"
      (lambda (t)

        (define (fold-left proc knil list)
          (fold (lambda (acc elt) (proc elt acc)) knil list))

        (define (test fn)
          (t.is (procedure? fn) true)
          (t.is (fn 4) 10))

        (let ((fn (curry (curry (curry + 1) 2) 3)))
          (test fn))

        (let ((fn (fold-left curry + '(1 2 3))))
          (test fn))))

(test "std: char properties"
      (lambda (t)
        ;; function taken from book Sketchy Scheme by Nils M Holm
        (define (char-properties x)
          (apply append
                 (map (lambda (prop)
                        (cond (((car prop) x)
                               (cdr prop))
                              (else '())))
                      (list (cons char-alphabetic? '(alphabetic))
                            (cons char-numeric? '(numeric))
                            (cons char-upper-case? '(upper-case))
                            (cons char-lower-case? '(lower-case))
                            (cons char-whitespace? '(whitespace))))))

        (t.is (map char-properties '(#\C #\c #\1 #\#))
              '((alphabetic upper-case)
                (alphabetic lower-case)
                (numeric)
                ()))))

(test "std: context in class instance"
      (lambda (t)
        (let ((result (vector)))
          (define-class Foo Object
            (constructor (lambda (self)
                           (set! self.x 10)))
            (closure (lambda (self)
                       (--> result (push (eq? this self)))
                       (lambda ()
                         ;; function will not have this like in JavaScript
                         ;; but get it from closure (parent scope)
                         ;; like with arrow function in ES6
                         (--> result (push (eq? this self)))
                         (--> result (push this.x))
                         this))))
          (let* ((x (new Foo))
                 (closure (x.closure))
                 (v (closure)))
            (t.is v.x 10)
            (t.is result #(true true 10))))))

;; bytevector tests from R7RS spec
(test "std: bytevector-u8-ref"
      (lambda (t)
        (define result (bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21)
                                          5))
        (t.is result 8)))

(test "std: bytevector-u8-set!"
      (lambda (t)
        (define result (let ((bv (bytevector 1 2 3 4)))
                         (bytevector-u8-set! bv 1 3)
                         bv))
        (t.is result #u8(1 3 3 4))))

(test "std: bytevector-u8-copy"
      (lambda (t)
        (define a #u8(1 2 3 4 5))
        (t.is (bytevector-copy a 2 4) #u8(3 4))))

(test "std: bytevector-copy!"
      (lambda (t)
        (define a (bytevector 1 2 3 4 5))
        (define b (bytevector 10 20 30 40 50))
        (bytevector-copy! b 1 a 0 2)
        (t.is a #u8(1 2 3 4 5))
        (t.is b #u8(10 1 2 40 50))))

(test "std: utf8->string"
      (lambda (t)
        (t.is (utf8->string #u8(#xCE #xBB)) "λ")
        (let ((v #u8(#xCE #xBB #x41 #x41 #x41)))
           (t.is (utf8->string v 0 2) "λ")
           (t.is (utf8->string v 0 4) "λAA")
           (t.is (utf8->string v 2 4) "AA"))))

(test "std: string->utf8"
      (lambda (t)
        (t.is (string->utf8 "λ") #u8(#xCE #xBB))
        (let ((str "λAA"))
          (t.is (string->utf8 str 0 1) #u8(#xCE #xBB))
          (t.is (string->utf8 str 0 2) #u8(#xCE #xBB #x41))
          (t.is (string->utf8 str 1 3) #u8(#x41 #x41)))))

(test "std: atanh and log function"
      (lambda (t)
        ;; source: https://doc.scheme.org/surveys/ComplexLog/
        (define (atanh x)
          (/ (- (log (+ 1 x))
                (log (- 1 x)))
             2))
        (t.is (atanh -2)
              -0.5493061443340548+1.5707963267948966i)))

(test.failing "std: Petrofsky let"
      (lambda (t)
        (t.is (let - ((n (- 1))) n) -1)))
