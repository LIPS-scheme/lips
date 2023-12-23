;; Copyright (C) Oleg Kiselyov (1999). All Rights Reserved.
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
;; source: https://srfi.schemers.org/srfi-10/srfi-10.html

(set-special! "#," 'sharp-comma)

(define **reader-ctor-list** '())

;; -----------------------------------------------------------------------------
(define (define-reader-ctor symbol fn)
  "(define-reader-ctor symbol fn)

   Define the value for SRFI-10 #, syntax.
   Example:

   (define-reader-ctor '+ +)
   (print #,(+ 1 2))"
  (let ((node (assoc symbol **reader-ctor-list**)))
    (if (pair? node)
        (set-cdr! node fn)
        (set! **reader-ctor-list** (cons (cons symbol fn)
                                         **reader-ctor-list**)))))

;; -----------------------------------------------------------------------------
(define-syntax sharp-comma
  (syntax-rules ()
    ((_ (fn arg ...))
     (let ((node (assoc 'fn **reader-ctor-list**)))
       (if (pair? node)
           ((cdr node) 'arg ...)
           (syntax-error (string-append "Invalid symbol " (symbol->string 'fn)
                                        " in expression " (repr '(fn arg ...))))))))
  "(sharp-comma expr) or #,(expr ...)

   This is syntax extension for SRFI-10. To define the function to be used with
   this syntax you need to call `define-reader-ctor` to and define a
   symbol-to-function mapping.")
