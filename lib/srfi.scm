;; -----------------------------------------------------------------------------
;; SRFI-2 https://srfi.schemers.org/srfi-2/srfi-2.html
;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; SRFI-10 https://srfi.schemers.org/srfi-10/srfi-10.html
;; -----------------------------------------------------------------------------
(set-special! "#," 'sharp-comma)

(define **reader-ctor-list** '())

;; -----------------------------------------------------------------------------
(define (define-reader-ctor symbol fn)
  "(define-reader-ctor symbol fn)

   Define the value for #, syntax. SRFI-10
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
  "(sharp-comma expr)
   #,(ctor ...)

   This is syntax extension for SRFI-10. To define the function to be used with
   This syntax you need to call `define-reader-ctor` function and define
   symbol function mapping.")
