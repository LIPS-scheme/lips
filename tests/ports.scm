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

(test "ports: input-string"
      (lambda (t)
        (let ((p (open-input-string "xy")))
          (t.is (list (peek-char p)
                      (read-char p)
                      (peek-char p)
                      (read-char p)
                      (peek-char p)
                      (read-char p))
                (list #\x #\x #\y #\y lips.eof lips.eof)))
        (let ((result (vector))
              (p (open-input-string "first line
                                     second line")))
          (result.push (read-line p))
          (result.push (read-line p))
          (t.is result #("first line" "second line")))))

(test "ports: repr"
      (lambda (t)
        (t.is (repr (current-input-port)) "#<input-port>")
        (t.is (repr (open-input-string "xxx")) "#<input-port <string>>")
        (t.is (repr (open-input-string "xxx")) "#<input-port <string>>")
        (t.is (repr (open-input-file "./tests/ports.scm")) "#<input-port ./tests/ports.scm>")))

(test "ports: input-port"
      (lambda (t)
        (let ((port (let* ((lines #("First Line" "Second Line" "Third Line"))
                           (i 0))
                      (lips.InputPort (lambda ()
                                        (if (>= i (vector-length lines))
                                            lips.eof
                                            (let ((line (vector-ref lines i)))
                                              (set! i (+ i 1))
                                              line)))))))
          (t.is (read-line port) "First Line")
          (t.is (read-line port) "Second Line")
          (t.is (read port) 'Third)
          (t.is (peek-char port) #\L)
          (t.is (read-char port) #\L)
          (t.is (read-char port) #\i)
          (t.is (read port) 'ne))))
