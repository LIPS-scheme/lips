#!/usr/bin/env lips

(define fs (require "fs"))

(define (get-list file)
  (--> (fs.promises.readFile file)
       (toString)
       (replace #/#.*/ "")
       (split "\n")
       (filter Boolean)
       (map (lambda (line)
              (--> line
                   (split #/ +/)
                   0)))))


(define (print-list file)
  (let ((list (get-list file)))
    (--> list (map (lambda (fn)
                        (if (not (bound? fn))
                            (print fn)))))))

(begin
  (print "R5RS")
  (print-list "../assets/R5RS_list")
  (print "R7RS")
  (print-list "../assets/R7RS_list"))
