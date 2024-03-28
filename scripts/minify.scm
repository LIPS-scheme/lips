#!/usr/bin/env lips

(define (dump expr)
  (let ((result (open-output-string)))
    (write expr result)
    (display (--> (get-output-string result)
                  (replace #/\n/g "\\xA;")))
    (newline)
    (close-port result)))

(let ((argv (command-line)))
  (if (= (length argv) 2)
      (let ((port (open-input-file (cadr argv))))
        (do ((expr (read port) (read port)))
          ((eof-object? expr))
          (dump expr)))))
