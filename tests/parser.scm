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
