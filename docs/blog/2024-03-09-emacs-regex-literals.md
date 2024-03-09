---
slug: emacs-scheme-regex
title: Scheme Regex literals in Emacs
authors: jcubic
tags: [scheme, emacs]
---

LIPS Scheme support regular expression literals, but it's not the only one implementation that
support those.  Other implementation includes [Gauche](https://practical-scheme.net/gauche/) and
[Kawa](https://www.gnu.org/software/kawa/index.html).

Unfortunetlly, you can easily use those regular expressions in [GNU
Emacs](https://en.wikipedia.org/wiki/GNU_Emacs), my main editor of choice.

<!--truncate-->

The main problem is when using vertical bar character inside Scheme code in Emacs. GNU Emacs thinks
that the vertical bar is part of the [symbol](/docs/scheme-intro/data-types#symbols):

```scheme
(let ((str "foo bar")
      (re #/foo|bar/)) ;; | ))
  (str.match re))
;; ==> #("foo")
```

This blog (the same as whole website) use modified PrismJS Scheme mode that supports regular
expressions. But in GNU Emacs there was a need to add `|` after a comment and close the lists that
were ignored by Emacs scheme mode (because they were inside symbol).

I asked a [question on emacs-devel mailing list](https://lists.gnu.org/archive/html/emacs-devel/2024-02/msg00896.html), on how to solve this problem. I didn't get any reply for days, then suddenly somone
[reply with this emacs lisp code snippet](https://lists.gnu.org/archive/html/emacs-devel/2024-03/msg00282.html).

```lisp
(defun scheme-regex-patch ()
  (setq-local
   syntax-propertize-function
   (lambda (start end)
     (goto-char start)
     (funcall
      (syntax-propertize-rules
       ;; For #/regexp/ syntax
       ("\\(#\\)/\\(\\\\/\\|\\\\\\\\\\|.\\)*?\\(/\\)"
        (1 "|")
        (3 "|"))
       ;; For #; comment syntax
       ("\\(#\\);"
        (1 (prog1 "< cn"
             (scheme-syntax-propertize-sexp-comment
              (point) end)))))
      (point) end))))

(add-hook 'scheme-mode-hook 'scheme-regex-patch)
```

And this solution works great.
