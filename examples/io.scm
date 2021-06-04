(let ((fname "tmp.txt"))
  (if (file-exists? fname)
      (error (string-append "file " fname " exists")))
  (call-with-output-file fname
    (lambda (output-port)
      (display "hello, world" output-port)
      (newline output-port)))

  (call-with-input-file fname
    (lambda (input-port)
      (let loop ((x (read-char input-port)))
        (if (not (eof-object? x))
            (begin
              (display x)
              (loop (read-char input-port)))))))

  (delete-file fname))
