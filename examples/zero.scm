#!/usr/bin/env -S lips -t

(define fs (require "fs"))

(define (read-zero n)
  (new Promise (lambda (resolve reject)
                 (fs.open "/dev/zero" "r" (lambda (err fd)
                             (if (null? err)
                                 (let* ((buffer (Buffer.from (new Array n)))
                                        (return (lambda (err num)
                                                   (if (null? err)
                                                       (resolve buffer)
                                                       (reject err)))))
                                   (fs.read fd buffer 0 buffer.length 0 return))
                                   (reject err)))))))

(console.log (read-zero 8))
