(test "ports: scheme repr (output-string)"
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

(test "ports: port repr"
      (lambda (t)
        (t.is (repr (current-input-port)) "#<input-port>")
        (t.is (repr (open-input-string "xxx")) "#<input-port <string>>")
        (t.is (repr (open-input-string "xxx")) "#<input-port <string>>")
        (t.is (repr (open-input-file "./tests/ports.scm")) "#<input-port ./tests/ports.scm>")))

(test "ports: syntax extensions"
      (lambda (t)
        (let ((p (open-input-string "#(1 2 3)")))
          (t.is (read p) #(1 2 3)))))

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

(test "ports: read input file"
      (lambda (t)
        (define f (open-input-file "./tests/stubs/test.txt"))
        (let ((line (read-line f)))
          (t.is line "Hello this is File"))))

(test "ports: read after port close should throw"
      (lambda (t)
        (define f (open-input-file "./tests/ports.scm"))
        (read f)
        (close-input-port f)
        (t.is (repr (open-input-file "./tests/ports.scm")) "#<input-port ./tests/ports.scm>")
        (t.is (to.throw (read f)) true)))

(test "ports: with-input-from-file"
      (lambda (t)
        (define stdin (current-input-port))
        (t.is (with-input-from-file "./tests/stubs/test.txt" read-line)
              "Hello this is File")
        (t.is (current-input-port) stdin)))

(test "ports: read/write/delete file"
      (lambda (t)
        (let* ((filename "./tests/__random__.scm")
               (input '(hello world))
               (p (open-output-file filename))
               (result #f))
          (write input p)
          (close-output-port p)
          (let ((p (open-input-file filename)))
            (set! result (read p))
            (delete-file filename)
            (t.is result input)))))
