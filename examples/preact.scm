;; -*- scheme -*-

(define Component (.. preact.Component))
(define render (.. preact.render))
(define h (.. preact.h))


(define (ls.get namespace)
  (--> localStorage (getItem namespace)))

(define (ls.set namespace data)
  (--> localStorage (setItem namespace (--> JSON (stringify data)))))

(define (store namespace data)
  (if data
      (ls.set namespace data)
      (ls.get namespace)))

(define-class Item Component
   (constructor (lambda (self)))
   (render (lambda (self props state)
             (with-tags (:li () (list
                                  (:span () (.. props.title))
                                  (:button (:onclick (lambda (e)
                                                       ((.. props.delete) (.. props.id))))
                                           "delete")))))))



(define-class Button Component
    (constructor (lambda (self))) ;; constructor is required - TODO: allow to skip
    (render (lambda (self props state)
              (with-tags (:button (:onclick (lambda (e)
                                              (alert "foo")))
                                  (.. props.name))))))

;;(define-class App Component
;;  (constructor (lambda (self)
;;                 (set-obj! self 'state '((items . '())))))
;;  (add-item (lambda (self)
;;              (let* ((state (.. self.state))
;;                     (items (cdr (assoc state 'items)))
;;                     (new-items (append 
;;                
;;                    
;;                (--> self (setState ((append state 
;;              
;;  (render (lambda 
;;
;;(render (with-tags (:div () (list (:ul ()
;;                                       
;;                             
;;        (. document 'body))

(render (with-tags (:div (:id "foo")
                         (list (:span () "Hello World")
                               (Button (:name "me")))))
        (. document 'body))
