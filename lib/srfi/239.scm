;; Copyright (C) Robby Zambito (2023).  All Rights Reserved.
;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax duplicate-clause
  (syntax-rules ()
    ((_ c) (syntax-error "duplicate clause in list-case" c))))

(define-syntax list-case
  (syntax-rules ()
    ((list-case expr clauses ...)
     (letrec-syntax
         ((make-clauses
           (syntax-rules ::: (_ pair null dotted matched)
             ((make-clauses obj p n d)
              (error "list-case: no matching clause" obj))

             ;; pair clauses
             ((make-clauses obj (matched pair) n d ((_ . _) body1 ::: body2) remaining :::)
              (duplicate-clause '(_ . _)))

             ((make-clauses obj pair n d ((_ . _) body1 ::: body2) remaining :::)
              (if (pair? obj)
                  (begin body1 ::: body2)
                  (make-clauses obj (matched pair) n d remaining :::)))

             ((make-clauses obj (matched pair) n d ((head . _) body1 ::: body2) remaining :::)
              (duplicate-clause '(head . _)))

             ((make-clauses obj pair n d ((head . _) body1 ::: body2) remaining :::)
              (if (pair? obj)
                  (let ((head (car obj)))
                    body1 ::: body2)
                  (make-clauses obj (matched pair) n d remaining :::)))

             ((make-clauses obj (matched pair) n d ((_ . tail) body1 ::: body2) remaining :::)
              (duplicate-clause '(_ . tail)))

             ((make-clauses obj pair n d ((_ . tail) body1 ::: body2) remaining :::)
              (if (pair? obj)
                  (let ((tail (cdr obj)))
                    body1 ::: body2)
                  (make-clauses obj (matched pair) n d remaining :::)))

             ((make-clauses obj (matched pair) n d ((head . tail) body1 ::: body2) remaining :::)
              (duplicate-clause '(head . tail)))

             ((make-clauses obj pair n d ((head . tail) body1 ::: body2) remaining :::)
              (if (pair? obj)
                  (let ((head (car obj)) (tail (cdr obj)))
                    body1 ::: body2)
                  (make-clauses obj (matched pair) n d remaining :::)))

             ;; null clauses
             ((make-clauses obj p (matched null) d (() body1 ::: body2) remaining :::)
              (duplicate-clause '()))

             ((make-clauses obj p null d (() body1 ::: body2) remaining :::)
              (if (null? obj)
                  (begin body1 ::: body2)
                  (make-clauses obj p (matched null) d remaining :::)))

             ;; dotted clauses
             ((make-clauses obj p n (matched dotted) (_ body1 ::: body2) remaining :::)
              (duplicate-clause '_))

             ((make-clauses obj p n dotted (_ body1 ::: body2) remaining :::)
              (if (and (not (null? obj))
                       (not (pair? obj)))
                  (begin body1 ::: body2)
                  (make-clauses obj p n (matched dotted) remaining :::)))

             ((make-clauses obj p n (matched dotted) (x body1 ::: body2) remaining :::)
              (duplicate-clause 'x))

             ((make-clauses obj p n dotted (x body1 ::: body2) remaining :::)
              (if (and (not (null? obj))
                       (not (pair? obj)))
                  (let ((x obj))
                    body1 ::: body2)
                  (make-clauses obj p n (matched dotted) remaining :::))))))
       (let ((obj expr))             ;only evaluate expr once
         (make-clauses obj pair null dotted clauses ...))))))
