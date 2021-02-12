#!/usr/bin/env lips

(if (= process.argv.length 4)
  (let* ((fs (require "fs"))
         (buff (fs.promises.readFile (. process.argv 3)))
         (port (open-input-string (--> buff (toString)))))
    (do ((expr (read port) (read port)))
      ((eq? expr eof))
      (write expr))))