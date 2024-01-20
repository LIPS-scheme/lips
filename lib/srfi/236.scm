(define-macro (independently . body)
  `(begin
    ,@(shuffle body)))