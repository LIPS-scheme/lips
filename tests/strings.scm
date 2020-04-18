(test "strings: sensitive"
  (lambda (t)
    (test-specs (string=? #t "foo" "foo")
                (string<=? #t "foo" "foo")
                (string>=? #t "foo" "foo")

                (string=? #f "foo" "fooo")
                (string<? #t "foo" "goo")
                (string<=? #t "foo" "goo")
                (string>? #t "goo" "foo")
                (string>=? #t "goo" "foo")

                (string>? #t "1234" "123")
                (string>? #t "124" "123")
                (string>=? #t "124" "123")
                (string<? #t "123" "124")
                (string<=? #t "123" "124"))))


(test "strings: insensitive"
  (lambda (t)
    (test-specs (string-ci=? #t "foo" "Foo")
                (string-ci<=? #t "foO" "Foo")
                (string-ci>=? #t "foO" "Foo")

                (string-ci=? #f "foO" "foOo")

                (string-ci<? #t "Foo" "goo")
                (string-ci<? #t "foo" "Goo")
                (string-ci<=? #t "Foo" "goo")
                (string-ci>? #t "go)" "Foo")
                (string-ci>=? #t "go)" "Foo")

                (string-ci>? #t "1234" "123")
                (string-ci>? #t "124" "123")
                (string-ci>=? #t "124" "123")
                (string-ci<? #t "123" "124")
                (string-ci<=? #t "123" "124"))))


(test "characters: sensitive"
      (lambda (t)
        (test-specs (char=? #t #\x #\x)
                    (char=? #f #\x #\X)

                    (char=? #t #\1 #\1)
                    (char=? #t #\2 #\2)
                    (char=? #t #\) #\))
                    (char=? #t #\( #\()
                    (char=? #t #\# #\#)

                    (char<=? #t #\A #\A)
                    (char>=? #t #\A #\A)

                    (char<=? #t #\A #\B)
                    (char>=? #t #\B #\A)

                    (char<? #f #\Y #\X)
                    (char>? #f #\X #\Y))))


(test "characters: insensitive"
      (lambda (t)
        (test-specs (char-ci=? #t #\x #\x)
                    (char-ci=? #t #\x #\X)

                    (char-ci=? #t #\1 #\1)
                    (char-ci=? #t #\1 #\1)
                    (char-ci=? #t #\) #\))
                    (char-ci=? #t #\( #\()
                    (char-ci=? #t #\# #\#)

                    (char-ci<=? #t #\a #\A)
                    (char-ci>=? #t #\a #\A)

                    (char-ci<=? #t #\a #\B)
                    (char-ci>=? #t #\b #\A)
                    (char-ci<=? #t #\A #\b)
                    (char-ci>=? #t #\B #\a)

                    (char-ci<? #f #\y #\X)
                    (char-ci>? #f #\c #\Y))))
