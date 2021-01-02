(test "ports: scheme repr"
      (lambda (t)
        (define (repr x . rest)
          (let ((port (open-output-string))
                (quote (if (null? rest) #f (car rest))))
            (if quote
                (write x port)
                (display x port))
            (get-output-string port)))

        (t.is (repr '(1 2 3)) "(1 2 3)")
        (t.is (repr '#(1 2 (3 4))) "#(1 2 (3 4))")
        (t.is (repr '(1 2 "foo")) "(1 2 foo)")
        (t.is (repr '(1 2 "foo") true) "(1 2 \"foo\")")))

