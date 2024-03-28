;; -*- scheme -*-
;;
;; TODO app using LIPS - Scheme based Powerful lips in JavaScript
;; Copyright (C) 2018-2020 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under the MIT license
;;
;; code based on jQuery TODO MVC
;; https://github.com/tastejs/todomvc/blob/gh-pages/examples/jquery/js/app.js
;;
;; ----------------------------------------------------------------------------------
;; Helper functions
;; ----------------------------------------------------------------------------------

(define (filter-todos completed)
  "return todos where completed fields is equal to argument"
  (filter (lambda (item)
            (eq? (cdr (assoc 'completed item)) completed)) todos))


(define (element-id element)
  "return data-id attribute value"
  (--> ($ element) (closest "li") (data "id")))


(define (find-todo element)
  "return single todo representation of a given element"
  (let ((id (element-id element)))
    (find (lambda (item) (string=? (cdr (assoc 'id item)) id)) todos)))


(define (on-todo fn element)
  "Invoke function for each todo"
  (let* ((todo (find-todo element)))
    (if (not (null? todo))
        (fn todo))))


(define (get-todos)
  "return selected todo"
  (if (string=? selected-filter "active")
    (filter-todos false)
    (if (string=? selected-filter "completed")
      (filter-todos true)
      todos)))


(define (uuid)
  "(uuid)

   Generatates unique ID"
  (let ((i 0) (rnd) (uuid ""))
    (while (< i 32)
      (set! rnd (|\|| (* (--> Math (random)) 16) 0))
            (if (or (== i 8) (== i 12) (== i 16) (== i 20))
              (set! uuid (concat uuid "-")))
            (let ((value (if (== i 1)
                           4
                           (if (== i 16)
                             (|\|| (|&| rnd 3) 8)
                             rnd))))
              (set! uuid (concat uuid (number->string (inexact->exact value) 16))))
            (++ i))
      uuid))


(define (pluralize count word)
  (if (or (> count 1) (== count 0)) (concat word "s") word))


(define (store namespace . data)
  "set or get value from localStorage"
  (if (not (null? data))
      (--> localStorage (setItem namespace (--> (car data) (toString))))
      (let ((store (--> localStorage (getItem namespace))))
        (if (null? store)
            '()
            (read store)))))

;; ref: https://www.programming-idioms.org/idiom/12/check-if-list-contains-a-value/154/scheme
(define (contains x list)
  (if (not list)
      false
      (if (equal? (car list) x)
          true
          (contains x (cdr list)))))


(define log (. console "log"))

;; ----------------------------------------------------------------------------------
;; Global variables
;; ----------------------------------------------------------------------------------
(define todos (store "todos-jquery"))
(define ENTER_KEY 13)
(define ESCAPE_KEY 27)
(define selected-filter "all")


;; ----------------------------------------------------------------------------------
;; Templates
;; ----------------------------------------------------------------------------------
(define (template selector)
  (let ((html (--> ($ selector) (html))))
    (--> Handlebars (compile html))))


(define todo-template (template "#todo-template"))
(define footer-template (template "#footer-template"))

;; ----------------------------------------------------------------------------------
;; Event handles
;; ----------------------------------------------------------------------------------

(define (toggle-completed)
  "remove completed todos"
  (let ((ids (--> ($ ".toggle:checked")
                  (closest "li")
                  (map (lambda (i item)
                         (--> ($ item) (data "id"))))
                  (get))))
    (set! todos (filter (lambda (item)
                           (not (--> ids (includes (cdr (assoc 'id item))))))
                   todos))
    (render)))


(define (add-todo e)
  "create new todo"
  (let* ((input ($ ".new-todo"))
         (val (--> (input.val) (trim))))
    (if (and (not (string=? val "")) (= e.which ENTER_KEY))
      (begin
        (set! todos (append! todos
                             (list `((id . ,(uuid))
                                     (title . ,val)
                                     (completed . false)))))
        (input.val "")
        (write todos)
        (newline)
        (render)))))


(define (destroy event)
  "remove todo"
  (let ((id (element-id (. event "target"))))
    (set! todos (filter (lambda (item)
                          (not (string=? (cdr (assoc 'id item)) id))) todos))
    (render)))


(define (toggle event)
  "flip completed on todo and render todos"
  (on-todo (lambda (todo)
             (let ((completed (cdr (assoc 'completed todo))))
               (set-cdr! (assoc 'completed todo) (not completed))
               (render)))
           (. event "target")))


(define (toggle-all event)
  (let ((is-checked (--> ($ (. event "target")) (prop "checked"))))
    (for-each (lambda (item) (set-cdr! (assoc 'completed item) is-checked)) todos)
    (render)))


(define (edit-mode event)
  (let* (($input (--> ($ (. event "target"))
                     (closest "li")
                     (addClass "editing")
                     (find ".edit")))
         (tmp-str (--> $input (val))))
    (--> $input (val ""))
    (--> $input (val tmp-str))
    (--> $input (focus))))


(define (edit-keyup event)
  (let ((key (. event "which"))
        (element (. event "target")))
    (if (eq? key ENTER_KEY)
      (--> element (blur)))
    (if (eq? key ESCAPE_KEY)
      (--> ($ element) (data "abort" true) (blur)))))


(define (update event)
  "update title of todo but not when user press ESC"
  (let* ((element (. event "target"))
         ($el ($ element))
         (val (--> $el (val) (trim))))
    (if (--> $el (data "abort"))
      (begin
        (--> $el (data "abort" false))
        (render))
      (if (not val)
        (destroy event)
        (begin
          (on-todo (lambda (todo)
                     (set-cdr! (assoc 'title todo) val))
                   element)
          (render))))))

;; ----------------------------------------------------------------------------------
;; Main code
;; ----------------------------------------------------------------------------------

(define (render-footer)
  "get data about toods and render the app footer"
  (let* ((todo-count (length todos))
         (active-count (length (filter-todos false)))
         (data `((activeTodoCount . ,active-count)
                 (activeTodoWord . ,(pluralize active-count "item"))
                 (completedTodos . ,(- todo-count active-count))))
         (template (footer-template (--> data (to_object)))))
    (--> ($ ".footer")
         (toggle (> todo-count 0))
         (html template))))


(define (render)
  "render the app"
  (let* ((todos (get-todos))
         (data (--> (map (lambda (e) (--> e (to_object))) todos) (to_array))))
    (--> ($ ".todo-list")
         (html (todo-template data)))
    (--> ($ ".main") (toggle (> (length data) 0)))
    (--> ($ ".new-todo") (focus))
    (render-footer)
    (let ((todos (filter-todos false)))
      (--> ($ ".toggle-all") (prop "checked" (== (length todos) 0))))
    (store "todos-jquery" todos)))

(define (bind)
  "bind event handlers"
  (--> ($ ".new-todo") (on "keyup" add-todo))
  (--> ($ ".toggle-all") (on "change" toggle-all))
  (--> ($ ".todoapp")
       (on "click" ".clear-completed" toggle-completed))
  (--> ($ ".todo-list")
       (on "dblclick" "label" edit-mode)
       (on "keyup" ".edit" edit-keyup)
       (on "focusout" ".edit" update)
       (on "change" ".toggle" toggle)
       (on "click" ".destroy" destroy)))




(define (init)
  "initialization function"
  (bind)
  (let* ((routes `(("/:filter" . ,(lambda (filter)
                                    (set! selected-filter filter)
                                    (render)))))
         (router (new Router (--> routes (to_object)))))
    (--> router (init "/all"))))

(init)
