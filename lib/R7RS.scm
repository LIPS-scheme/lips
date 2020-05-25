;; -*- scheme -*-
;; Attempt to add missing R7RS small standard functions and macros
;;
;; Reference:
;; https://small.r7rs.org/attachment/r7rs.pdf
;;
;; This file is part of the LIPS - Scheme based Powerful LISP in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license

;; -----------------------------------------------------------------------------
(define-macro (do vars test . body)
  (let ((return? (not (symbol? (car test)))))
    `(let (,@(map (lambda (spec)
                    `(,(car spec) ,(cadr spec)))
                 vars))
        (while (not ,(if return? `,(car test) `,test))
          ,@body
          ,@(map (lambda (spec)
                   `(set! ,(car spec) ,(caddr spec)))
              (filter (lambda (item)
                         (== (length item) 3))
                    vars)))
        ,(if return? (cadr test)))))
