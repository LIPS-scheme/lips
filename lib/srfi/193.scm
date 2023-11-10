;;   __ __                          __
;;  / / \ \       _    _  ___  ___  \ \
;; | |   \ \     | |  | || . \/ __>  | |
;; | |    > \    | |_ | ||  _/\__ \  | |
;; | |   / ^ \   |___||_||_|  <___/  | |
;;  \_\ /_/ \_\                     /_/
;;
;; <https://lips.js.org>
;;
;; implementation of SRFI-193 https://srfi.schemers.org/srfi-193/srfi-193.html
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2021 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;

;; -----------------------------------------------------------------------------
(define (command-name)
  "(command-name)

   Returnss LIPS script that was executed or #f."
  (let* ((cmd (command-line))
         (name (car cmd)))
    (and (> (length name) 0) name)))

;; -----------------------------------------------------------------------------
(define (command-args)
  "(command-args)

   Returnss list of arguments for LIPS script that was executed."
  (let* ((cmd (command-line))
         (args (cdr cmd)))
    (if (null? args)
        nil
        args)))

;; -----------------------------------------------------------------------------
(define (script-file)
  "(script-file)

   Returnss absolute path for LIPS script that was executed or #f."
  (if (eq? self window)
      #f
      (let ((name (command-name)))
        (and name (path.resolve name)))))

;; -----------------------------------------------------------------------------
(define (script-directory)
  "(script-file)

   Returnss list of absolute path for directory of LIPS script that was executed or #f."
  (if (eq? self window)
      #f
      (let ((path (require "path"))
            (filename (script-file)))
        (and filename (path.dirname filename)))))
