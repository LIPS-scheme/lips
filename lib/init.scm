;; -----------------------------------------------------------------------------
;; init internal fs for LIPS Scheme Input/Output functions
;; -----------------------------------------------------------------------------
(let* ((fs (cond ((eq? self global) (require "fs"))
                 ((and (not (null? self.BrowserFS)) (indexed-db?))
                  (new Promise (lambda (resolve reject)
                                 (BrowserFS.configure
                                  &(:fs "IndexedDB"
                                        :options &())
                                  (lambda (e)
                                    (if (null? e)
                                        (resolve (BrowserFS.BFSRequire "fs"))
                                        (reject e)))))))
                 ((not (null? self.BrowserFS))
                  (console.warn (string-append "BrowserFS not initilalized "
                                               "IndexedDB is not available"))
                  nil)))
       (Buffer (cond ((eq? self global)
                      self.Buffer)
                     ((not (null? self.BrowserFS))
                      (. (BrowserFS.BFSRequire "buffer") 'Buffer)))))
  (let ((internal (lips.env.get '**internal-env**)))
    (if (not (null? Buffer))
        (internal.set "Buffer" Buffer))
    (if (not (null? fs))
        (internal.set "fs" fs))))
