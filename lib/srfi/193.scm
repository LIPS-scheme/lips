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

(define (command-name)
  "(command-name)
  
   Function returns LIPS script that was executed."
  (let ((cmd (command-line)))
    (if (null? cmd)
        #f
        (car cmd))))

(define (command-args)
  "(command-args)
  
   Function returns list of arguments for LIPS script that was executed."
  (let ((cmd (command-line)))
    (if (null? cmd)
        nil
        (cdr cmd))))

(define (script-file)
  "(script-file)
  
   Function returns list of absolute path for LIPS script that was executed."
  (if (eq? self window)
      ""
      (let ((path (require "path"))
            (cmd (command-line)))
         (if (null? cmd)
             ""
            (path.resolve (car cmd))))))

(define (script-directory)
  "(script-file)
  
   Function returns list of absolute path for directory of LIPS script that was executed."
  (if (eq? self window)
      ""
      (let ((path (require "path"))
            (filename (script-file)))
        (path.dirname filename))))