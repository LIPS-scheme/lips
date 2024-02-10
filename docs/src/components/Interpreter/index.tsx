import Head from '@docusaurus/Head';
import { useLayoutEffect } from 'react';
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
        description: 'Filter function accept, regex or function. Curry is higher order function that create new function with defaults.',
        code: `(print (filter number? '(foo 10 bar 20 baz 30)))
;; ==> (10 20 30)
(print (filter #/foo|bar/ '(foo bar baz quux)))
;; ==> (foo bar)
(define foo_or_bar (curry filter #/foo|bar/))
(print (foo_or_bar '(foo bar baz)))
;; ==> (foo bar)`
    },
    {
        description: 'Automagic async/await like resolving of promises and explicit promise quotation.',
        code: `(define h1-re #/<h1>([^>]+)<\\/h1>/)
;; automagic promise resolving
(let ((msg (--> (fetch "https://api.scheme.org/")
                (text)
                (match h1-re)
                1)))
  (print msg))

;; explicit promise handling with quotation
(let ((promise (--> '>(fetch "https://api.scheme.org/")
                    (then (lambda (res)
                            (res.text)))
                    (then (lambda (x)
                            (. (x.match h1-re) 1))))))
  (print (await promise)))`
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
        code: `;; macro that swap first two arguments
;; with the last two expressions
(define-macro (swap! a b x y)
  (let ((g_b (gensym)))
    \`(let ((,g_b ,y))
       (set! ,a ,b)
       (set! ,b ,g_b))))

;; example taken from Go website
;; fib crates a function
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
        code: `;; recursive syntax rules macro
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
        description: 'SRFI-139 syntax-parameterize allows creating anamorphic hygienic macros.',
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
    useLayoutEffect(() => {
        (function loop() {
            if (scripts.length) {
                const script = scripts.shift();
                getScript(script).then(loop);
            }
        })();
    }, []);
}


export default function Interpreter(): JSX.Element {
    useScripts([
        'https://code.jquery.com/jquery-3.4.1.min.js',
        'https://cdn.jsdelivr.net/combine/npm/jquery.terminal/js/jquery.terminal.min.js,npm/js-polyfills/keyboard.js,npm/prismjs/prism.js,npm/jquery.terminal/js/prism.js,npm/prismjs/components/prism-scheme.min.js',
        'https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/terminal.js',
        'https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/prism.js',
        '/js/interpreter.js'
    ]);
    return (
        <>
          <Head>
            <link href="https://cdn.jsdelivr.net/combine/npm/jquery.terminal/css/jquery.terminal.min.css,npm/prismjs/themes/prism-coy.css,npm/terminal-prism/css/prism-coy.css" rel="stylesheet"/>
            <link href="https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/css/terminal.css"
                  rel="stylesheet"/>
            <link href="https://cdn.jsdelivr.net/gh/richleland/pygments-css/monokai.css"
                  rel="stylesheet"/>
            {/*
            <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"
                    bootstrap="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/std.xcb"/>
            */}
            <script src="http://localhost/~kuba/jcubic/scheme/lips/dist/lips.js"
                    bootstrap="http://localhost/~kuba/jcubic/scheme/lips/dist/std.scm" />
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
            <div className="term"/>
            <div className="examples">
              <button className="run">run</button>
              <ul className="list">
                {examples.map((example, index) => {
                    return (
                        <li key={index}>
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
                    return <li key={index}><a href="#">{ index + 1 }</a></li>;
                })}
              </ul>
            </div>
          </div>
        </>
    );
};