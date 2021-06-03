(test "compiler: should create same list structure"
      (lambda (t)
        (define data '(foo bar "100" 10 100+20i 10.2 #\x #/foo bar/gi))
        (let* ((serialized (lips.serialize data))
               (deserialized (lips.unserialize serialized)))
          (t.is data deserialized))))
