
(test "quasiquote: it should return list"
      (lambda (t)
        (t.is (eval (let ((x '((list 1 2 3) (list 1 2 3) (list 1 2 3))))
                      `(list `(,@,(car x)))))
              '((1 2 3)))))
