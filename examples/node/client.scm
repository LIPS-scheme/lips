#!/usr/bin/env lips

(define http (require "http"))
(define https (require "https"))

(define url.parse (. (require "url") 'parse))

(define **global** (current-environment))

(define (get-chunks obj)
  (new Promise (lambda (resolve)
                 (let ((data ""))
                   (obj.setEncoding "utf8")
                   (obj.on "data" (lambda (chunk)
                                    (set! data (string-append data chunk))))
                   (obj.on "end" (lambda ()
                                   (resolve data)))))))

(define (post url data)
  (new Promise (lambda (resolve reject)
                 (let* ((url (url.parse url))
                        (req (http.request &(:host url.hostname
                                             :port url.port
                                             :path url.path
                                             :method "POST"
                                             :headers &(:Content-Type "text/plain"
                                                        :Content-Length (length data)))
                                           (lambda (res)
                                             (resolve (get-chunks res))))))
                   (req.write data)
                   (req.end)))))

(define (get url)
  (new Promise (lambda (resolve reject)
                 (let* ((parsed (url.parse url))
                        (request (if (string=? parsed.protocol "https")
                                     https.get
                                     http.get)))
                   (request url (lambda (res)
                                       (resolve (get-chunks res))))))))

(define (make-rpc url)
  (try (let ((data (read (get url))))
         (let iter ((methods (cdr (assoc 'methods data))))
           (if (not (null? methods))
               (let* ((method (car methods))
                     (name (string-append "rpc." (symbol->string method))))
                 (--> **global** (set name (lambda args
                                             (post url (repr `((method . ,method) (params . ,args))
                                                             true)))))
                 (iter (cdr methods))))))
       (catch (e)
              (display e.message)
              (newline))))

(make-rpc "http://localhost:4200/rpc")
(display (rpc.add 10 20))
(newline)
(display (rpc.echo "hello World"))
(newline)

