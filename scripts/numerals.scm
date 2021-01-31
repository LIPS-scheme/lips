#!/usr/bin/env lips

(define fs (require "fs"))

(define (get-data)
  (let ((path (string-append __dirname "/../assets/UnicodeData.txt")))
    (new Promise (lambda (resolve reject)
                    (fs.readFile path (lambda (err data)
                                         (if (null? err)
                                             (resolve (data.toString))
                                             (reject err))))))))

(define (get-zeros data)
  (--> data
       (split "\n")
       (filter (lambda (line)
                  (line.match #/ZERO;Nd;/
                              )))
       (map (lambda (line)
               (let ((parts (line.split ";")))
                 (number->string (string->number (. parts 0) 16)))))))



(print (string-append "(define *zero-number-chars* #("
                      (--> (get-zeros (get-data)) (join " "))
                      "))"))
