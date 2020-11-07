(test "env: unset & parent env"
      (lambda (t)
        (define (string-append str)
           (let ((string-append (--> lips.env.__parent__ (get "string-append"))))
             (string-append str " world")))
        (let ((msg (string-append "hello")))
          (t.is msg "hello world"))

        (unset! string-append)
        (let ((msg (string-append "lorem" " " "ipsum")))
          (t.is msg "lorem ipsum"))))
