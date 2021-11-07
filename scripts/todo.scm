#!/usr/bin/env lips

(define fs (require "fs"))
(define path (require "path"))

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

(define dir (path.dirname (path.resolve (car (command-line)))))

(begin
  (print "R5RS")
  (print-list (path.join dir "../assets/R5RS_list"))
  (print "R7RS")
  (print-list (path.join dir "../assets/R7RS_list")))
