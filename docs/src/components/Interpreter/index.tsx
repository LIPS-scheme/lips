import Head from '@docusaurus/Head';
import { useEffect } from 'react';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

import './styles.css';

const examples = [
    {
        description: 'Mixing Scheme and JavaScript',
        code: `;; &() is object literal used with quasiquote
(let ((object \`&(:name "LIPS Scheme"
                 :version ,lips.version)))
  ;; you can access JavaScript properties
  ;; with dot notation
  (print (string-append object.name
                        " "
                        object.version))
  ;; you can mix scheme and JavaScript
  (ignore (setTimeout (lambda ()
                (alert (JSON.stringify object)))
               1000)))`
    },
    {
        description: 'Filter function accept, regex or function. Curry is higher order function that create new function with defaults. Pluck return a function that extract fields from an object.',
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
        description: 'Automagic async/await like resolving of promises and explicit promise quotation.',
        code: `;; JavaScript regular expression literal
(define re #/<h1>([^>]+)<\\/h1>/)
;; --> is a macro that allow chaining
;; of JavaScript methods calls
;; no need to use Promises becasue of automagic
;; promise resolving
(let ((msg (--> (fetch "https://scheme.org.pl")
                (text)
                (match re)
                1)))
  (print msg))

;; explicit promise handling with quotation
(let ((promise (--> '>(fetch "https://scheme.org.pl")
                    (then (lambda (res)
                            (res.text)))
                    (then (lambda (x)
                            (. (x.match re) 1))))))
  (print (await promise)))`
    },
    {
        description: 'Hygienic syntax-rules macro and few examples of Numeric Tower.',
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
(print P:("jon"))
;; ==> P:("jon")
(print (. P:("jon") 'name))
;; ==> "jon"`
    },
    {
        description: 'Interaction with JavaScript DOM and jQuery Terminal (REPL).',
        code: `;; this will query the DOM and ivoke click method
(let ((selector "button[class*=\\"ColorModeToggle\\"]"))
  (--> (document.querySelector selector)
       (click)))

;; accessing jQuery Terminal, ignore works like begin
;; but the return value is ignored so the terminal
;; is not paused when it find a Promise from
;; Terminal typing animation
(ignore
  (term.css "--background" "#2E2E2E")
  (term.echo "This is LIPS Scheme" &(:typing #t)))`
    },
    {
        description: 'Fibonacci Closure with swap! lisp style macro.',
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
        description: 'JavaScript generators (objects that implement iterator protocol)',
        code: `;; JavaScript generator create using JS eval
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
        description: 'Y Combinator and inline factorial function.',
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
    }
];

function getScript(script: string) {
    return new Promise((resolve, reject) => {
        const $script = document.createElement("script");
        $script.onload = resolve;
        $script.onerror = reject;
        $script.src = script;
        document.head.appendChild($script);
    });
}

function useScripts(scripts: string[]) {
    useEffect(() => {
        (function loop() {
            if (scripts.length) {
                const script = scripts.shift();
                getScript(script).then(loop);
            }
        })();
    }, []);
}


export default function Interpreter(): JSX.Element {
    const { siteConfig } = useDocusaurusContext();
    //const prod = process.env.NODE_ENV === 'production';
    useScripts([
        'https://code.jquery.com/jquery-3.7.1.min.js',
        'https://cdn.jsdelivr.net/combine/npm/jquery.terminal/js/jquery.terminal.min.js,npm/js-polyfills/keyboard.js,npm/prismjs/prism.js,npm/jquery.terminal/js/prism.js,npm/prismjs/components/prism-scheme.min.js',
        'https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/terminal.js',
        'https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/prism.js',
        `${siteConfig.baseUrl}js/interpreter.js`
    ]);
    return (
        <>
          <Head>
            <link rel="preconnect" href="https://cdn.jsdelivr.net" />
            <link href="https://cdn.jsdelivr.net/combine/npm/jquery.terminal/css/jquery.terminal.min.css,npm/prismjs/themes/prism-coy.css,npm/terminal-prism/css/prism-coy.css" rel="stylesheet"/>
            <link href="https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/css/terminal.css"
                  rel="stylesheet"/>
            <link href="https://cdn.jsdelivr.net/gh/richleland/pygments-css/monokai.css"
                  rel="stylesheet"/>
            {/*prod && <>
              <script src="https://code.jquery.com/jquery-3.7.1.min.js" />
              <script src="https://cdn.jsdelivr.net/combine/npm/jquery.terminal/js/jquery.terminal.min.js,npm/js-polyfills/keyboard.js,npm/prismjs/prism.js,npm/jquery.terminal/js/prism.js,npm/prismjs/components/prism-scheme.min.js" />
              <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/terminal.js" />
              <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/prism.js" />
              <script src={`${siteConfig.baseUrl}/js/interpreter.js`} />
            </>*/}
            <script src="https://cdn.jsdelivr.net/gh/jcubic/lips@devel/dist/lips.min.js"
                    data-bootstrap="https://cdn.jsdelivr.net/gh/jcubic/lips@devel/dist/std.xcb"/>
          </Head>
          <div className="intro">
            <div className="actions-wrapper">
              <ul className="actions cloak">
                <li className="zoom-in icon">
                  <a href="#">Zoom In</a>
                </li>
                <li className="zoom-out icon">
                  <a href="#">Zoom Out</a>
                </li>
                <li className="full-screen">
                  <ul>
                    <li className="full-screen icon">
                      <a href="#">Full Screen</a>
                    </li>
                    <li className="exit-full-screen icon">
                      <a href="#">Exit Full Screen</a>
                    </li>
                  </ul>
                </li>
              </ul>
            </div>
            <div className="term">
              <div className="loader-container">
                <div className="loader">
                  <div>.</div>
                  <div>..</div>
                  <div>...</div>
                  <div>....</div>
                  <div>.....</div>
                  <div>......</div>
                </div>
              </div>
            </div>
            <div className="examples">
              <button className="run">run</button>
              <ul className="list">
                {examples.map((example, index) => {
                    return (
                        <li key={index} className={index === 0 ? 'active' : undefined}>
                          <div className="example">
                            <pre>{example.code}</pre>
                          </div>
                          <div className="description">{example.description}</div>
                        </li>
                    );
                })}
              </ul>
              <ul className="pagination">
                {examples.map((_, index) => {
                    return (
                        <li key={index} className={index === 0 ? 'active' : undefined}>
                          <a href="#">{ index + 1 }</a>
                        </li>
                    );
                })}
              </ul>
            </div>
          </div>
        </>
    );
};
