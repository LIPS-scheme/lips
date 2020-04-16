

(define parent.frames__t1
   ((lambda ()
      (define x 10)
      (car (map (lambda () (parent.frames)) '(1))))))
            
(define parent.frames__t2 (map (lambda () (parent.frame)) '(1 2 3)))

(define (parent.frames_foo)
  (define x 10)
  (parent.frames__bar))

(define (parent.frames__bar)
  (define x 20)
  (parent.frames__baz))

(define (parent.frames__baz)
  (map (lambda (env)
          (let-env env
             x))
        (parent.frames)))


(define parent.frames__t3 (parent.frames_foo))

(test "parent.frames: map+lambda in function"
      (lambda (t)
         (t.is (let-env (car parent.frames__t1) x) 10)))

(test "parent.frames: map+lambda"
      (lambda (t)
         (t.is parent.frames__t2 '(() () ()))))

(test "parent.frames: stack of function calls"
      (lambda (t)
        (t.is parent.frames__t3 '(10 20))))
