(test_ "TOC: it should run (! 1000)"
  (define (! n)
    (let iter ((n n) (acc 1))
      (if (<= n 0)
          acc
          (iter (- n 1) (* acc n))))))
