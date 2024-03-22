#!/usr/bin/env lips

(define fs (require "node:fs/promises"))

(define (get-data)
  (let ((path (string-append __dirname "/../assets/CaseFolding.txt")))
    (fs.readFile path "utf8")))

(define (get-mapping data)
  (--> data
       (split "\n")
       (filter (lambda (line)
                  (and (not (string=? line ""))
                       (null? (line.match #/^#/))
                       (not (null? (line.match #/; C/
                                               ))))))
       (map (lambda (line)
              (let* ((parts (line.split "; "))
                     (small (not (null? (line.match #/SMALL/))))
                     (upper (string->number (. parts (if small 2 0)) 16))
                     (lower (string->number (. parts (if small 0 2)) 16)))
                (string-append "\"" (number->string upper) "\""  ": " (number->string lower)))))))




(print (string-append "const fold_case_mapping = {"
                      (--> (get-mapping (get-data))
                           (reduce (lambda (acc expr index arr)
                                     (if (zero? index)
                                         (string-append "\n    " expr ", ")
                                         (let ((suffix (if (= (- arr.length 1) index) "" ", ")))
                                           (if (zero? (remainder index 4))
                                               (string-append acc "\n    " expr suffix)
                                               (string-append acc expr suffix)))))
                                         ""))
                      "\n}"))"))
