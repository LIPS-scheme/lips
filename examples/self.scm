#!/usr/bin/env lips

(define path (require "path"))

(define cwd (process.cwd))

(let ((fs (. (require "fs") "promises")))
  (print (--> (fs.readFile "self.scm") (toString))))
