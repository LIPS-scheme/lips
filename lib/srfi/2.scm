;; Copyright (C) Marc Nieper-Wißkirchen, Public Domain
;;
;; ref: https://github.com/scheme-requests-for-implementation/srfi-2/tree/master/contrib

(define-syntax and-let*
  (syntax-rules ()
    ((_ ())
     #t)
    ((and-let () form form* ...)
     (begin form form* ...))
    ((_ ((id expr)))
     expr)
    ((_ ((expr)))
     expr)
    ((_ (id))
     id)
    ((_ ((id expr) . claw*) . body)
     (let ((id expr))
       (and id (and-let* claw* . body))))
    ((_ ((expr) . claw*) . body)
     (and expr (and-let* claw* . body)))
    ((_ (id . claw*) . body)
     (and id (and-let* claw* . body)))
    ((_ . _)
     (syntax-error "ill-formed and-let* form")))
  "(and-let* ((name expression) expression ...) body)

   Combines let and and. First expression needs to be in form of let.
   Symbol expression the rest can be boolean expression or name expreession.
   This is implementation of SRFI-2.")
