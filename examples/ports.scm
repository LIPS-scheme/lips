;; https://srfi.schemers.org/srfi-6/srfi-6.html

(let ((q (open-output-string))
      (x '(a b c)))
  (display (car x) q)
  (display (cdr x) q)
  (get-output-string q))

(let ((p (open-input-string "(a . (b . (c . ()))) 34")))
  (display (read p))
  (display (read p))
  (display (eof-object? (peek-char p))))

(let ((buff (open-output-string))
      (stdout (current-output-port)))
  (display "hello" buff)
  (display " " buff)
  (display "world" buff)
  (display (get-output-string buff) stdout))
