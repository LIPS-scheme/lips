#!/usr/bin/env lips

(define (dump expr)
  (let ((result (open-output-string)))
    (write expr result)
    (display (--> (get-output-string result)
                  (replace #/\n/g "\\xA;")))
    (newline)
    (close-port result)))

(if (= process.argv.length 4)
    (let ((port (open-input-file (. process.argv 3))))
      (do ((expr (read port) (read port)))
        ((eof-object? expr))
        (dump expr))))