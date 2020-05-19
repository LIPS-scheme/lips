(test "parser: syntax extension"
      (lambda (t)
        (add-special! "<>" 'html lips.specials.SPLICE)
        (define-macro (html . args)
          (let ((str (--> (list->array (map symbol->string args)) (join "+"))))
            `(string-append "<" ,str "/>")))

        (t.is (eval (read "<>(foo bar)") (current-environment)) "<foo+bar/>")
        (remove-special! "<>")

        (add-special! "--" 'dash lips.specials.LITERAL)
        (define-macro (dash x)
          `'(,(car x) . (,cadr x)))

        (t.is (eval (read "--(foo bar baz)") (current-environment)) '(foo . bar))

        (t.is (read "(--)") '((dash)))
        (t.is (read "(-- x)") '((dash x)))
        (t.is (read "--x") '(dash x))
        (remove-special! "--")
        (t.is (read "(--)") '(--))))
