#!/usr/bin/env -S lips --debug -t

(load "./helpers.scm")

(suite (add "Regex: variable" (lambda ()
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
                                             (str.match #/foo/))))))))

