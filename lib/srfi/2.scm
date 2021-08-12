;; Copyright (C) Oleg Kiselyov (1998). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.
;;
;; source: https://srfi.schemers.org/srfi-2/srfi-2.html

(define-syntax and-let*
  (syntax-rules ()
    ((_ ()) #t)
    ((_ () body ...)
     (let () body ...))
    ((_ ((expression))) ;; last/single expression
     expression)
    ((_ ((symbol expression)) body ...) ;; last/single pair
     (let ((symbol expression))
       (if symbol (begin body ...))))
    ((_ ((symbol expression) expr ...) body ...) ;; lead pair
     (let ((symbol expression))
       (and symbol (and-let* (expr ...) body ...))))
    ((_ ((expression) expr ...) body ...) ;; lead expression
     (and expression (and-let* (expr ...) body ...))))
  "(and-let* ((name expression) expression ...) body)

   Macro that combine let and and. First expression need to be in form of let.
   Symbol expression the rest can be boolean expression or name expreession.
   This is implementation of SRFI-2.")
