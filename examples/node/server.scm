#!/home/kuba/projects/jcubic/lips/bin/lips.js

(define express (require "express"))
(define app (express))
(define body-parser (require "body-parser"))
(define __dirname (process.cwd))


(app.use (express.static __dirname))

(app.use (lambda (req res next)
            (let ((data ""))
              (req.setEncoding "utf8")
              (req.on "data" (lambda (chunk)
                                (set! data (string-append data chunk))))
              (req.on "end" (lambda ()
                               (set-obj! req 'body data)
                               (next))))))

(define **methods** '())

(define-macro (define-method spec . body)
  (let ((name (car spec)))
     (if (assoc name **methods**)
         `(set-cdr (assoc ',name **methods**) (lambda ,(cdr spec) ,@body))
         `(set! **methods** (cons (cons ',name (lambda ,(cdr spec) ,@body)) **methods**)))))


(define-method (echo str)
   str)

(define-method (add a b)
  (+ a b))

(get "/"
    (lambda (req res)
       (res.sendFile (concat __dirname "/" "index.html"))))

(--> app
     (get "/rpc"
          (lambda (req res)
              (res.send (repr '((methods . (echo add))))))))
(--> app
     (post "/rpc"
          (lambda (req res)
             (try (let* ((data (read req.body))
                         (method (cdr (assoc 'method data)))
                         (args (cdr (assoc 'params data))))
                    (display data)
                    (newline)
                    (let ((fn (assoc method **methods**)))
                      (if fn
                          (let ((result (apply (cdr fn) args)))
                            (res.send (repr result true)))
                          (throw "method not found"))))
                (catch (e)
                   (res.send (repr `((error . ,e.message)))))))))

(app.listen 4200 (lambda ()
                    (console.log "server at localhost:4200")))
