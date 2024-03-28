;; Source
;; http://www.willdonnelly.net/blog/runtime-scheme-detection/

;;; DETECT
;;; A set of functions to allow an interpreted Scheme
;;; program to determine the implementation it is
;;; running under.

;; DETECT:SIGNATURE
;; Assemble a signature of the current
;; Scheme implementation.
(define (detect:signature)
  (list
    ;; AXCH: exact-sqrt
    (exact? (sqrt 4))
    ;; AXCH: exact-times-zero
    (exact? (* 0 3.1))
    ;; AXCH: exact-div-zero
    (exact? (/ 0 4.7))
    ;; AXCH: exact-rationals
    (exact? (/ 1 3))
    ;; AXCH: case-sensitive
    (eq? 'a 'A)
    ;; AXCH: promises-are-thunks
    (procedure? (delay 3))
    ;; Do strings made from numbers less than 1 omit the 0?
    (string=? ".5" (number->string 0.5))
    ;; AXCH: literal-rationals
    (number? (string->number "1/2"))
    ;; AXCH: literal-complexes
    (number? (string->number "1+i"))
    ;; Is the empty string eqv to itself?
    (eqv? "" "")
    ;; How about the empty vector?
    (eqv?  '#() '#())
    ;; A non-empty string?
    (eqv? "a" "a")
    ;; Does SET! have a constant return value?
    (let ((x 0)) (eqv? (set! x 1) (set! x 'asd)))
    ;; Is it equal to other undefined things?
    (eqv? (for-each (lambda (x) #t) '(0 1 2)) (let ((x 123)) (set! x 321)))
    ;; Are negative and positive inexact zero the same?
    (eq? +0.0 -0.0)
    (eqv? +0.0 -0.0)
    (equal? +0.0 -0.0)
    ;; Is the default vector filled with zeroes?
    (equal? (make-vector 5) '#(0 0 0 0 0))
    ;; Is the default vector filled with falses?
    (equal?  (make-vector 5) '#(#f #f #f #f #f))
    ;; Vector-fill returns a vector?
    (vector? (vector-fill! (make-vector 1) 0)) ))

;; DETECT:KNOWN-SIGNATURES
;; A precalculated list of signatures for all supported
;; Scheme implementations.
(define detect:known-signatures
'((mzscheme   (#t #t #t #t #f #f #f #t #t #f #f #f #t #t #f #f #f #t #f #t))
  (chicken    (#f #f #f #f #f #f #f #t #f #f #f #f #t #t #f #t #t #f #f #f))
  (guile      (#f #t #f #t #f #f #f #t #t #t #f #f #t #t #f #f #t #f #f #f))
  (bigloo     (#f #f #f #f #f #t #f #f #f #f #f #f #t #t #f #t #t #f #f #f))
  (gambit     (#t #t #t #t #f #f #t #t #t #f #f #f #t #t #f #f #f #t #f #f))
  (ikarus     (#f #f #f #t #f #t #f #t #f #f #f #f #t #f #f #t #t #t #f #f))
  (scheme48   (#f #f #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #f))
  (mit-scheme (#t #t #t #t #t #f #t #t #t #f #t #f #f #f #f #t #t #f #t #f))
  (lips       (#t #f #f #t #f #t #f #t #t #f #f #f #t #t #f #f #f #t #t #f))
  (gauche     (#f #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #t #f #f #f))))

;; DETECT:MATCH-SIGNATURE
;;   Determine the name of the current Scheme implementation
;;    by checking the signature returned by DETECT:SIGNATURE
;;    against a table of known signatures.
(define (detect:match-signature)
  (let ((signature (detect:signature)))
    ;; Loop over the DETECT:KNOWN-SIGNATURES list
    (let test ((siglist detect:known-signatures))
      (if (equal? '() siglist)
          ;; Return 'UNKNOWN if we're stumped
          'unknown
          (let ((testsig (car siglist)))
            (if (equal? (cadr testsig) signature)
                (car testsig)
                (test (cdr siglist))))))))

;; DETECT:NAME
;; Memoized form of DETECT:MATCH-SIGNATURE
(define detect:name
  (let ((memo #f))
    (lambda ()
      (and (not memo)
           (set! memo (detect:match-signature)))
      memo)))
