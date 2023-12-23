(define Benchmark (require "benchmark"))


(define-macro (suite . body)
   `(let ((suite (new Benchmark.Suite)))
      (--> suite ,@body
                 (run &(:async true))

                 (on "cycle" (lambda (e)
                               (console.log e.target)
                               (print (string-append ">>> " (e.target.toString)))))

                 (on "complete" (lambda (e)
                                  (try
                                    (print (string-append "Fastest is "
                                                          (repr (--> (this.filter "fastest")
                                                                     (map "name")
                                                                     0))))
                                  (catch (e)
                                    (print e))))))))
