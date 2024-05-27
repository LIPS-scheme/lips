;; parametrize tests based on https://docs.racket-lang.org/guide/parameterize.html
(test "parameterize: lexical"
      (lambda (t)
        (define location (make-parameter "here"))

        (t.is (location) "here")

        (t.is (parameterize ([location "there"]) (location))
              "there")

        (t.is (parameterize ([location "in a house"])
                (list (location)
                      (parameterize ([location "with a mouse"])
                        (location))
                      (location)))
              '("in a house" "with a mouse" "in a house"))))

(test "parametrize: closures"
              (lambda (t)
                (define location (make-parameter "here"))

                (let ([get (parameterize ([location "with a fox"])
                             (lambda () (location)))])
                  (t.is (get) "here"))))

(test "parametrize: change value"
      (lambda (t)
        (define location (make-parameter "here"))

        (t.is (list (location) (begin (location "there")
                                      (location)))
              '("here" "there"))))

(test "parametrize: change value + lexical"
              (lambda (t)
                (define location (make-parameter "here"))

                (define (try-again! where)
                  (location where))

                (t.is (parameterize ([location "on a train"])
                        (list (location)
                              (begin (try-again! "in a boat")
                                     (location))))
                      '("on a train" "in a boat"))))


(test "parametrize: force/delay"
      (lambda (t)
        ;; example taken from SRFI-155
        (t.is (let ()
                (define x (make-parameter 1))
                (define p (delay (x)))
                (define (g p) (parameterize ((x 2)) (force p)))
                (+ (force p) (g p)))
              2)))

;; TODO
;; throw error when parameterize got no pair
;; get more than one parameter
;; use parameterze with invalid parameter
;; make-parameter throws with non function
