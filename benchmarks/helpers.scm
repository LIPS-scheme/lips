(define Benchmark (require "benchmark"))


(define-macro (suite . body)
   `(let ((suite (new Benchmark.Suite)))
      (--> suite ,@body
                 (run &(:async true))

                 (on "cycle" (lambda (e)
                               (print (string-append ">>> " (String target)))))

                 (on "complete" (lambda (e)
                                  (try
                                    (print (string-append "Fastest is "
                                                          (repr (--> (this.filter "fastest")
                                                                     (map "name")
                                                                     0))))
                                  (catch (e)
                                    (print e))))))))