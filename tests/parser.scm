(test "parser: syntax extension"
      (lambda (t)
        (set-special! "<>" 'html lips.specials.SPLICE)
        (define-macro (html . args)
          (let ((str (--> (list->array (map symbol->string args)) (join "+"))))
            `(string-append "<" ,str "/>")))

        (t.is (eval (read "<>(foo bar)") (current-environment)) "<foo+bar/>")
        (unset-special! "<>")

        (set-special! "--" 'dash lips.specials.LITERAL)
        (define-macro (dash x)
          `'(,(car x) . (,cadr x)))

        (t.is (eval (read "--(foo bar baz)") (current-environment)) '(foo . bar))

        (t.is (read "(--)") '((dash)))
        (t.is (read "(-- x)") '((dash x)))
        (t.is (read "--x") '(dash x))
        (unset-special! "--")
        (t.is (read "(--)") '(--))))

(test "parser: escape hex literals"
      (lambda (t)
         (t.is (to.throw (read "\"\\x9\"")) #t)
         (t.is "\uFFFF" "￿")
         (t.is "\x9;\x9;" "\t\t")
         (t.is '|\x9;\x9;|  '|\t\t|)))

(test "parser: quotes with literals"
      (lambda (t)
        (t.is ''#f '(quote #f))
        (t.is ''#x10 '(quote #x10))
        (t.is ''#o10 '(quote #o10))
        (t.is ''#b10 '(quote #b10))

        ;; binary
        (t.is ''#i#b10 '(quote #i#b10))
        (t.is ''#b#i10 '(quote #b#i10))
        (t.is ''#e#b10 '(quote #e#b10))
        (t.is ''#b#e10 '(quote #b#e10))

        ;; hex
        (t.is ''#i#x10A '(quote #i#x10A))
        (t.is ''#x#i10A '(quote #x#i10A))
        (t.is ''#e#x10A '(quote #e#x10A))
        (t.is ''#x#e10A '(quote #x#e10A))

        ;; octal
        (t.is ''#i#o10 '(quote #i#o10))
        (t.is ''#o#i10 '(quote #o#i10))
        (t.is ''#e#o10 '(quote #e#o10))
        (t.is ''#o#e10 '(quote #o#e10))))

(test "parser: it should ignore comments"
      (lambda (t)

        (t.is (list #;(foo bar (quux)) 10 20) (list 10 20))
        (t.is (list #;foo 10 20) (list 10 20))
        (t.is (list 10 #;10+10i 20) (list 10 20))
        (t.is (list 10 ;foo bar
                    20)
              (list 10
                    20))))

(test "parser: it should return literal space"
      (lambda (t)
        (let ((str (make-string 10 #\ )))
          (t.is (string-length str) 10)
          (t.is (not (null? (--> str (match /^\s{10}$/)))) #t))))
