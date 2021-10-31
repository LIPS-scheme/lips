#!/usr/bin/env lips

(define (symbolic? a)
  "(symbolic? a)

   Check if string passed as argument is symbol like."
  (string=? (a.toLowerCase) (a.toUpperCase)))

(define (name-compare a b)
  "(name-compare a b)

   Comparator for sort that ensure that symbols are in front."
  (cond ((or (and (symbolic? a)
                  (symbolic? b))
             (and (not (symbolic? a))
                  (not (symbolic? b))))
         (a.localeCompare b))
        ((and (symbolic? a)
              (not (symbolic? b)))
         -1)
        ((and (not (symbolic? a))
              (symbolic? b))
         1)))

(let* ((global-env lips.env.__parent__)
       (names (list->vector  (map symbol->string (env global-env)))))
  (names.sort name-compare)
  (print "# Function Reference")
  (print "")
  (--> names (forEach (lambda (name)
                        (let ((value (global-env.get name)))
                          (when (and (procedure? value) (null? (name.match #/^%./)))
                            (print (string-append "## " name))
                            (print "```")
                            (print (eval `(help ,(string->symbol name)) global-env))
                            (print "```")
                            (print "")))))))
