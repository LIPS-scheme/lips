const examples = [
  {
    description: `With LIPS you can mix Scheme and JavaScript. You can access all JavaScript objects. You can also call any functions from Scheme.

**NOTE**: you can hover over Scheme symbols and get tooltip with documentation.`,
    code: `;; &() is object literal used with quasiquote
(let ((obj \`&(:name "LIPS Scheme"
              :version ,lips.version)))
  ;; you can access JavaScript properties
  ;; with dot notation, print is display + newline
  (print (string-append obj.name
                        " "
                        obj.version))
  ;; you can mix scheme and JavaScript
  (setTimeout (lambda ()
                (alert (JSON.stringify obj)))
              1000)
  #void)`
  },
  {
    description: 'Filter function accepts: regex or function. Curry is higher order function that creates a new function with default arguments. Pluck returns a function that extract fields from an object.',
    code: `(print (filter number? '(foo 10 bar 20 baz 30)))
;; ==> (10 20 30)
(print (filter #/foo|bar/ '(foo bar baz quux)))
;; ==> (foo bar)
(define foo_or_bar (curry filter #/foo|bar/))
(print (foo_or_bar '(foo bar baz)))
;; ==> (foo bar)

;; &() is object literal used with quasiquote
(define (make-person first last)
  \`&(:fist ,first :last ,last))

(define beatles (map make-person
                    '("John" "Paul" "Ringo" "George")
                    '("Lennon" "McCartney"
                      "Starr" "Harrison")))
;; pluck will extract properties from objects
(write (map (pluck "fist") beatles))
(newline)
;; ==> ("John" "Paul" "Ringo" "George")`
  },
  {
    description: 'LIPS do automagic async/await. It resolve all promises by default, but you can quote a promise and handle it like in JavaScript.',
    code: `;; JavaScript regular expression literal
(define re #/<h1>([^>]+)<\\/h1>/)
;; --> is a macro that allow chaining
;; of JavaScript methods calls
;; no need to use Promises because of automagic
;; promise resolving
(let ((msg (--> (fetch "https://scheme.org.pl/test/")
                (text)
                (match re)
                1)))
  (print msg))

;; explicit promise handling with quotation
(let ((promise (--> '>(fetch "https://scheme.org.pl/test/")
                    (then (lambda (res)
                            (res.text)))
                    (then (lambda (x)
                            (. (x.match re) 1))))))
  (print (await promise)))`
  },
  {
    description: 'You can use hygienic syntax-rules macro, here are also a few examples of Numeric Tower.',
    code: `;; show hygienic macro prints expression
;; and the result value
(define-syntax show
  (syntax-rules ()
    [(_ expr ...)
     (begin
       (begin
         (write 'expr)
         (display " = ")
         (write expr)
         (newline))
         ...)]))

;; few example of Numeric Tower
(show (/ 1 2)
      (expt 1/4 1/2)
      (expt 10+10i 1/2)
      (log 2+1/2i)
      (acos -1)
      (+ 1/2 1/4)
      (* 3/4 1/10))`
  },
  {
    description: 'LIPS also supports classic lisp macros. Here is example of anaphoric macro (a lambda shortcut)',
    code: `(define-macro (λ . body)
   \`(lambda ($1 $2 $3 $4 $5 $6 $7 $8 $8 $9) ,@body))

(print (map (λ (+ $1 1)) (range 10)))
;; ==> (1 2 3 4 5 6 7 8 9 10)
(let ((x 10))
  (print (map (λ (+ $1 $2))
              (range x)
              (cdr (range (+ x 1))))))
;; ==> (1 3 5 7 9 11 13 15 17 19)`
  },
  {
    description: 'Syntax extensions and custom repr allow to create new homoiconic data types.',
    code: `;; Create new class using define-class macro
(define-class Person Object
   (constructor (lambda (self name)
                   (set! self.name name))))

;; add syntax extension
(set-special! "P:" 'make-person lips.specials.SPLICE)

;; add class representation
(set-repr! Person
  (lambda (x q)
    (string-append "P:(" (repr x.name q) ")")))

;; function that create new object
;; for the syntax extension
(define (make-person name)
   (new Person name))

;; we can use new syntax
(write P:("jon"))
(newline)
;; ==> P:("jon")
(print (. P:("jon") 'name))
;; ==> "jon"`
  },
  {
    description: 'With LIPS you can interact with JavaScript DOM and jQuery Terminal (REPL).',
    code: `;; this will query the DOM and invoke click method
(let ((selector "[class*=\\"colorModeToggle\\"] button"))
  (--> (document.querySelector selector)
       (click)))

;; accessing jQuery Terminal, ignore works like begin
;; but the return value is ignored so the terminal
;; is not paused when it find a Promise from
;; Terminal typing animation
(ignore (term.animation
          (lambda ()
            (let (($button ($ ".run")))
              ($button.attr "disabled" #t)
              (term.css "--background" "#2E2E2E")
              (term.echo "This is LIPS Scheme" &(:typing #t))
              ($button.removeAttr "disabled")))))`
  },
  {
    description: 'Dynamic variables with R7RS parameterize',
    code: `;; define new dynamic parameter
(define x (make-parameter 10))
(define (double-x)
  (* (x) (x)))
;; use default value
(print (double-x))
;; ==> 100
;; change the value dynamically
(parameterize ((x 20))
  (print (double-x)))
;; ==> 400
`
  },
  {
    description: 'Here is a fibonacci Closure with swap! lisp style macro.',
    code: `;; macro that swap first two variables
;; with the last two expressions
(define-macro (swap! a b x y)
  (let ((g_b (gensym)))
    \`(let ((,g_b ,y))
       (set! ,a ,b)
       (set! ,b ,g_b))))

;; example taken from Go website
;; fib creates a function
;; that return fibonacci numbers
(define (fib)
   (let ((a 0) (b 1))
     (lambda ()
        (swap! a b b (+ a b))
        a)))

(let ((f (fib)))
  (list (f) (f) (f) (f) (f)))`
  },
  {
    description: 'Scheme hygienic macro that creates an assoc list, with macroexpand.',
    code: `;; recursive hygienic syntax-rules macro
(define-syntax alist
  (syntax-rules ()
     ((_) ())
     ((_ x y z ...)
      (cons (cons x y) (alist z ...)))))

(print (alist "foo" 10 "bar" 20 "baz" 30))
;; ==> ((foo . 10) (bar . 20) (baz . 30))
(macroexpand (alist "foo" 10 "bar" 20))
;; ==> (#:cons (#:cons "foo" 10)
;;             (#:cons (#:cons "bar" 20)
;;                     ()))`
  },
  {
    description: 'Function that modify its source code when run',
    code: `(define (repeater x)
   "(repeater value)

    Function prints the value 1 time and modifies itself
     to repeat (+ n 1) times on the next call."
   (for-each (lambda () (print x)) (range 1))
   (let ((r (cadr (cdadddr (. repeater '__code__)))))
     (set-cdr! r (list (+ (cadr r) 1)))))

(print "1")
(repeater 'hello)
(print "2")
(repeater 'hello)
(print "3")
(repeater 'hello)`
  },
  {
    description: 'Built in SRFI-139 syntax-parameterize allows creating anamorphic hygienic macros.',
    code: `;; define new syntax parameter
(define-syntax-parameter it
   (syntax-rules ()
     ((_ . _)
      (syntax-error "abort used outside of a loop"))))

;; syntax-rules macro aif adds (it) parameter
;; to access tested value.
(define-syntax aif
  (syntax-rules ()
    ((_ cond rest ...)
     (let ((test cond))
       (syntax-parameterize
        ((it (syntax-rules ()
               ((_) test))))
        (if test
            rest ...))))))

;; no need to use assoc two times
;; or using a variable to hold the value
(let ((alist '((a . 10) (b . 20))))
  (aif (assoc 'a alist)
      (print (cdr (it)))))`
  },
  {
    description: 'You can iterate over JavaScript generators (objects that implement iterator protocol)',
    code: `;; JavaScript generator created using JS eval
(define gen (self.eval "
    (async function* gen(time, ...args) {
        function delay(time) {
            return new Promise((resolve) => {
                setTimeout(resolve, time);
            });
        }
        for (let x of args) {
            await delay(time);
            yield x;
        }
    })"))

;; iteration over iterator/generator
(do-iterator
  (i (apply gen 100 (range 10)))
  ()
  (print i))
(print (iterator->array (gen 100 1 2 3 4 5)))
;; strings and lists are JavaScript iterators
(write (iterator->array "hello"))
(newline)
(print (iterator->array '(1 2 3 4)))`
  },
  {
    description: 'Example of Y Combinator and inline factorial function.',
    code: `(define Y
  (lambda (h)
    ((lambda (x) (x x))
     (lambda (g)
       (h (lambda args (apply (g g) args)))))))

((Y (lambda (f)
     (lambda (n)
       (cond ((< n 0)
              (throw (new Error "Invalid factorial")))
             ((zero? n) 1)
             (else (* n (f (- n 1))))))))
 10)
;; ==> 3628800`
  },
  {
    hidden: true,
    code: `(define-macro (%promisify expr)
  (let ((resolve (gensym "resolve")))
    \`(new Promise
       (lambda (,resolve)
          ,(append expr (list resolve))))))

(if (null? self.$.terminal.from_ansi)
    (%promisify ($.getScript "https://cdn.jsdelivr.net/npm/jquery.terminal/js/unix_formatting.js")))
(if (null? self.qrcode)
    (%promisify ($.getScript "https://cdn.jsdelivr.net/gh/jcubic/static/js/qrcode.js")))
(let ((code (%promisify (qrcode.generate "https://tinyurl.com/fxv87gb"))))
  (term.echo (code.replace #/\\[47m/g "[30;47m") &(:ansi #t))
  #void)`
  }
];


export default examples;
