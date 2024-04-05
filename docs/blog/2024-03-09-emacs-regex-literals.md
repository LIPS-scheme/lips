---
slug: emacs-scheme-regex
title: Scheme Regex literals in Emacs
authors: jcubic
image: /img/emacs-scheme-regex.png
tags: [scheme, emacs]
---

LIPS Scheme support regular expression literals, but it's not the only one implementation that
support those.  Other implementation includes [Gauche](https://practical-scheme.net/gauche/) and
[Kawa](https://www.gnu.org/software/kawa/index.html).

Unfortunetlly, you can't easily use those regular expressions in [GNU
Emacs](https://en.wikipedia.org/wiki/GNU_Emacs), my main editor of choice.

<!--truncate-->

## The problem

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

## The solution

I asked a [question on emacs-devel mailing
list](https://lists.gnu.org/archive/html/emacs-devel/2024-02/msg00896.html), on how to solve this
problem. I didn't get any reply for days, then suddenly someone [reply with this emacs lisp code
snippet](https://lists.gnu.org/archive/html/emacs-devel/2024-03/msg00282.html).

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

This solution worked great, until I've found that it don't properly handle Scheme expression
comments `#;`, that are part of the solution. In the meantime on the mailing list there was discussion
about this feature (probably because it's part of GNU Kawa) to integrate with builtin `scheme.el`.
So soon you may not need a hack like this when working with regular expressions.

This is a proposed solution after I said that the code doesn't work for Scheme expression comments.

```lisp
(defun scheme-regex-patch ()
   (setq-local
    syntax-propertize-function
    (lambda (beg end)
      (goto-char beg)
      (scheme-syntax-propertize-sexp-comment2 end)
      (scheme-syntax-propertize-regexp end)
      (funcall
       (syntax-propertize-rules
        ("\\(#\\);" (1 (prog1 "< cn"
                         (scheme-syntax-propertize-sexp-comment2 end))))
        ("\\(#\\)/" (1 (when (null (nth 8 (save-excursion
                                            (syntax-ppss
                                             (match-beginning 0)))))
                         (put-text-property
                          (match-beginning 1)
                          (match-end 1)
                          'syntax-table (string-to-syntax "|"))
                         (scheme-syntax-propertize-regexp end)
                         nil)
                       )))
       (point) end))))

(defun scheme-syntax-propertize-sexp-comment2 (end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

(defun scheme-syntax-propertize-regexp (end)
  (let* ((state (syntax-ppss))
         (within-str (nth 3 state))
         (start-delim-pos (nth 8 state)))
    (when (and within-str
               (char-equal ?# (char-after start-delim-pos)))
      (while
          (and
           (re-search-forward "/" end 'move)
           (eq -1
               (% (save-excursion
                    (backward-char)
                    (skip-chars-backward "\\\\")) 2))))
      (when (< (point) end)
        (progn
          (put-text-property
           (match-beginning 0)
           (match-end 0)
           'syntax-table (string-to-syntax "|")))))))

(add-hook 'scheme-mode-hook 'scheme-regex-patch)
```

You can read the whole discussion on [emacs-devel mailing list archive](https://lists.gnu.org/archive/html/emacs-devel/2024-03/msg00590.html).
