"use strict";(self.webpackChunknew_docs=self.webpackChunknew_docs||[]).push([[9342],{7177:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>r,contentTitle:()=>o,default:()=>u,frontMatter:()=>c,metadata:()=>s,toc:()=>l});var i=t(5893),a=t(1151);const c={sidebar_position:7,description:"Powerful feature of Scheme that allow to add new control flows"},o="Continuations",s={id:"scheme-intro/continuations",title:"Continuations",description:"Powerful feature of Scheme that allow to add new control flows",source:"@site/docs/scheme-intro/continuations.md",sourceDirName:"scheme-intro",slug:"/scheme-intro/continuations",permalink:"/docs/scheme-intro/continuations",draft:!1,unlisted:!1,editUrl:"https://github.com/LIPS-scheme/lips/tree/master/docs/docs/scheme-intro/continuations.md",tags:[],version:"current",sidebarPosition:7,frontMatter:{sidebar_position:7,description:"Powerful feature of Scheme that allow to add new control flows"},sidebar:"tutorialSidebar",previous:{title:"Streams",permalink:"/docs/scheme-intro/streams"},next:{title:"What Next",permalink:"/docs/scheme-intro/next-step"}},r={},l=[{value:"What is continuation?",id:"what-is-continuation",level:2},{value:"Accessing current continuation",id:"accessing-current-continuation",level:2},{value:"Calling continuations",id:"calling-continuations",level:2},{value:"Early exit",id:"early-exit",level:2},{value:"Loops",id:"loops",level:2},{value:"Generators",id:"generators",level:2}];function d(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",p:"p",pre:"pre",...(0,a.a)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.h1,{id:"continuations",children:"Continuations"}),"\n",(0,i.jsx)(n.h2,{id:"what-is-continuation",children:"What is continuation?"}),"\n",(0,i.jsx)(n.p,{children:"In Scheme and Lisp a continuation is a thing that is waiting for an expression to be evaluated."}),"\n",(0,i.jsx)(n.p,{children:"If you have code like this:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(+ 1 2 <slot>)\n"})}),"\n",(0,i.jsxs)(n.p,{children:["and ",(0,i.jsx)(n.code,{children:"<slot>"})," is an expression: (e.g.: ",(0,i.jsx)(n.code,{children:"(/ 1 10)"}),")"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(+ 1 2 (/ 1 10))\n"})}),"\n",(0,i.jsxs)(n.p,{children:["then continuation for expression ",(0,i.jsx)(n.code,{children:"(/ 1 10)"})," is ",(0,i.jsx)(n.code,{children:"(+ 1 2 <slot>)"}),". Scheme is unique because it allows\nto access continuations. They are first class objects like numbers or functions."]}),"\n",(0,i.jsx)(n.h2,{id:"accessing-current-continuation",children:"Accessing current continuation"}),"\n",(0,i.jsxs)(n.p,{children:["To access the current continuation for expression, you use ",(0,i.jsx)(n.code,{children:"call-with-current-continuation"})," or its\nabbreviation ",(0,i.jsx)(n.code,{children:"call/cc"}),". The procedure ",(0,i.jsx)(n.code,{children:"call/cc"})," accept a single procedure that get the continuation as\nfirst argument:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(call/cc (lambda (c)\n           ...))\n"})}),"\n",(0,i.jsxs)(n.p,{children:["The continuation saved in ",(0,i.jsx)(n.code,{children:"c"})," capture whole state of the Scheme interpreter. The continuation act as\na procedure that you can pass a single value to it and Scheme will jump in to the place where\ncontinuation was captured with a given value."]}),"\n",(0,i.jsx)(n.h2,{id:"calling-continuations",children:"Calling continuations"}),"\n",(0,i.jsx)(n.p,{children:"You can save continuation inside a variable and call it later like a procedure."}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define k #f)\n\n(+ 1 (call/cc\n       (lambda (continuation)\n         (set! k continuation)\n         2)))\n;; ==> 3\n(k 10)\n;; ==> 11\n"})}),"\n",(0,i.jsxs)(n.p,{children:["Here when you call a continuation ",(0,i.jsx)(n.code,{children:"k"})," with value 10 it restores the state in ",(0,i.jsx)(n.code,{children:"(+ 1 <slot>)"})," and\nexecute that expression again with a value ",(0,i.jsx)(n.code,{children:"10"}),"."]}),"\n",(0,i.jsxs)(n.p,{children:["The continuation act like a procedure and return ",(0,i.jsx)(n.code,{children:"#t"})," with ",(0,i.jsx)(n.code,{children:"procedure?"})," predicate:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define k (call/cc (lambda (c) c)))\n(procedure? k)\n;; ==> #t\n"})}),"\n",(0,i.jsx)(n.h2,{id:"early-exit",children:"Early exit"}),"\n",(0,i.jsx)(n.p,{children:"The simple thing you can do with continuations is an early exit. Scheme doesn't have a return\nexpression, but with continuations you can add one."}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define (find item lst)\n  (call/cc (lambda (return)\n             (let loop ((lst lst))\n                (if (null? lst)\n                    (return #f)\n                    (if (equal? item (car lst))\n                        (return lst)\n                        (loop (cdr lst))))))))\n"})}),"\n",(0,i.jsx)(n.p,{children:"You can even create abstrcation with anaphoric macro:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define-macro (alambda args . body)\n  `(lambda ,args\n     (call/cc (lambda (return)\n                ,@body))))\n"})}),"\n",(0,i.jsxs)(n.p,{children:["and you can use this macro like normal ",(0,i.jsx)(n.code,{children:"lambda"}),", but you have anaphoric ",(0,i.jsx)(n.code,{children:"return"})," expression:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define exists? (alambda (item lst)\n                         (for-each (lambda (x)\n                                     (if (equal? x item)\n                                         (return #t)))\n                                   lst)\n                         #f))\n\n(exists? 'x '(a b c d e f))\n;; ==> #f\n(exists? 'd '(a b c d e f))\n;; ==> #t\n"})}),"\n",(0,i.jsx)(n.p,{children:"Here for-each always iterates over all elements, but with early exit it will return immediately when\nfound a value."}),"\n",(0,i.jsx)(n.h2,{id:"loops",children:"Loops"}),"\n",(0,i.jsx)(n.p,{children:"You can create loops with continuations:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define (make-range from to)\n  (call/cc\n   (lambda (return)\n     (let ((result '()))\n       (let ((loop (call/cc (lambda (k) k))))\n         (if (<= from to)\n             (set! result (cons from result))\n             (return (reverse result)))\n         (set! from (+ from 1))\n         (loop loop))))))\n\n(make-range 1 10)\n;; ==> (1 2 3 4 5 6 7 8 9 10)\n"})}),"\n",(0,i.jsxs)(n.p,{children:["The first continuation creates an early exit, like in the previous example. But the second call/cc use\nidentity function (it return continuation). Which means that the continuation is saved in a loop\nvariable. And each time it's called with ",(0,i.jsx)(n.code,{children:"loop"})," as an argument, it's again assigned that\ncontinuation to loop variable. This is required for the next loop."]}),"\n",(0,i.jsx)(n.h2,{id:"generators",children:"Generators"}),"\n",(0,i.jsxs)(n.p,{children:["Some languages have generators and a ",(0,i.jsx)(n.code,{children:"yield"})," keyword. In Scheme, you can create generators with\ncontinuations."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define (make-coroutine-generator proc)\n  (define return #f)\n  (define resume #f)\n  (define yield (lambda (v)\n                  (call/cc (lambda (r)\n                             (set! resume r)\n                             (return v)))))\n  (lambda ()\n    (call/cc (lambda (cc)\n               (set! return cc)\n               (if resume\n                   (resume (if #f #f))  ; void? or yield again?\n                   (begin (proc yield)\n                          (set! resume (lambda (v)\n                                         (return (eof-object))))\n                          (return (eof-object))))))))\n"})}),"\n",(0,i.jsxs)(n.p,{children:["The above example came from\n",(0,i.jsx)(n.a,{href:"https://github.com/scheme-requests-for-implementation/srfi-158/blob/master/srfi-158-impl.scm#L77-L87",children:"SRFI 158 example implementation"}),"."]}),"\n",(0,i.jsxs)(n.p,{children:["The procedure ",(0,i.jsx)(n.code,{children:"make-coroutine-generator"})," allows defining generators:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define counter (make-coroutine-generator\n                 (lambda (yield)\n                   (do ((i 0 (+ i 1)))\n                     ((<= 3 i))\n                     (yield i)))))\n\n(counter) ;; ==> 0\n(counter) ;; ==> 1\n(counter) ;; ==> 2\n(counter) ;; ==> #<eof>\n"})}),"\n",(0,i.jsx)(n.p,{children:"With continuations, you can do a lot of cool new flow control structures."})]})}function u(e={}){const{wrapper:n}={...(0,a.a)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}},1151:(e,n,t)=>{t.d(n,{Z:()=>s,a:()=>o});var i=t(7294);const a={},c=i.createContext(a);function o(e){const n=i.useContext(c);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:o(e.components),i.createElement(c.Provider,{value:n},e.children)}}}]);