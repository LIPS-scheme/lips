#!/usr/bin/env -S lips --debug

(define Benchmark (require "benchmark"))


(define suite (new Benchmark.Suite))


(--> suite (add "Regex: variable" (lambda ()
                                    (let ((str "foo")
                                          (re #/foo/))
                                      (--> (new Array 1000)
                                           (fill 0)
                                           (map (lambda ()
                                                  (str.match re)))))))

           (add "Regex: literal" (lambda ()
                                   (let ((str "foo"))
                                     (--> (new Array 1000)
                                          (fill 0)
                                          (map (lambda ()
                                                 (str.match #/foo/)))))))

           (on "cycle" (lambda (e)
                           (print (string-append ">>> " (String e.target)))))
                           

           (on "complete" (lambda (e)
                            (try
                              (print (string-append "Fastest is "
                                                    (repr (--> (this.filter "fastest")
                                                               (map "name")
                                                               0))))
                            (catch (e)
                              (print e)))))




           (run &(:async true)))




