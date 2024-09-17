"use strict";(self.webpackChunknew_docs=self.webpackChunknew_docs||[]).push([[8133],{75:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>a,contentTitle:()=>o,default:()=>h,frontMatter:()=>t,metadata:()=>c,toc:()=>l});var i=r(4848),s=r(8453);const t={sidebar_position:4,description:"Environments in LIPS are first class objects"},o="Environments",c={id:"lips/environments",title:"Environments",description:"Environments in LIPS are first class objects",source:"@site/docs/lips/environments.md",sourceDirName:"lips",slug:"/lips/environments",permalink:"/docs/lips/environments",draft:!1,unlisted:!1,editUrl:"https://github.com/LIPS-scheme/lips/tree/master/docs/docs/lips/environments.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4,description:"Environments in LIPS are first class objects"},sidebar:"tutorialSidebar",previous:{title:"SXML (e.g. for React)",permalink:"/docs/lips/sxml"},next:{title:"Functional and other utils",permalink:"/docs/lips/functional-helpers"}},a={},l=[{value:"Environment chain",id:"environment-chain",level:2},{value:"Changing environment without eval",id:"changing-environment-without-eval",level:2},{value:"Scheme Report Environments",id:"scheme-report-environments",level:2},{value:"Introspection",id:"introspection",level:2},{value:"Frames",id:"frames",level:2},{value:"Global environment",id:"global-environment",level:2}];function d(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",p:"p",pre:"pre",strong:"strong",...(0,s.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.h1,{id:"environments",children:"Environments"}),"\n",(0,i.jsxs)(n.p,{children:["Environments in LIPS are first class objects that you can interact with.\nScheme spec define procedure ",(0,i.jsx)(n.code,{children:"(interactive-environment)"})," LIPS add also ",(0,i.jsx)(n.code,{children:"(current-environment)"}),"."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(let ((x 10))\n  (eval '(+ x x) (current-environment)))\n;; ==> 20\n"})}),"\n",(0,i.jsx)(n.h2,{id:"environment-chain",children:"Environment chain"}),"\n",(0,i.jsx)(n.p,{children:"Environments are created in a chain. In JavaScript there is a notion of scope chain the same,\nis in Scheme. But with environment object you can access that chain."}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(let ((x 10))\n  (let ((x 20))\n    (print (eval '(+ x x) (current-environment)))\n    (print (eval '(+ x x) (. (current-environment) '__parent__)))))\n;; ==> 40\n;; ==> 20\n"})}),"\n",(0,i.jsx)(n.h2,{id:"changing-environment-without-eval",children:"Changing environment without eval"}),"\n",(0,i.jsxs)(n.p,{children:["In LIPS you can change the environment with ",(0,i.jsx)(n.code,{children:"let-env"})," syntax:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define env (let ((x 10) (y 20))\n              (current-environment)))\n\n(let-env env\n  (+ x y))\n;; ==> 30\n"})}),"\n",(0,i.jsx)(n.h2,{id:"scheme-report-environments",children:"Scheme Report Environments"}),"\n",(0,i.jsxs)(n.p,{children:["Scheme standard provide environments for given version of the ",(0,i.jsxs)(n.a,{href:"/docs/scheme-intro/what-is-lisp#standards",children:["R",(0,i.jsx)("sup",{children:"n"}),"RS\nspecification"]})," in a form of a function\n",(0,i.jsx)(n.code,{children:"scheme-report-environment"}),"."]}),"\n",(0,i.jsxs)(n.p,{children:["You can use this function in LIPS with version 5 and 7 to get R",(0,i.jsx)("sup",{children:"5"}),"RS or R",(0,i.jsx)("sup",{children:"7"}),"RS."]}),"\n",(0,i.jsxs)(n.p,{children:[(0,i.jsx)(n.strong,{children:"NOTE"}),": that some of the functions from R",(0,i.jsx)("sup",{children:"5"}),"RS may have features of R",(0,i.jsx)("sup",{children:"7"}),"RS since\nsome of them got additional arguments. R",(0,i.jsx)("sup",{children:"n"}),"RS is backward compatible."]}),"\n",(0,i.jsxs)(n.p,{children:["You can use this function with ",(0,i.jsx)(n.code,{children:"eval"})," or ",(0,i.jsx)(n.code,{children:"let-env"}),":"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(let ((env (scheme-report-environment 7)))\n  (let-env env\n     (display (+ 1 2))))\n;; ==> 3\n(let-env (scheme-report-environment 7)\n  (display (--\x3e \"string\" (toUpperCase))))\n;; ==> Unbound variable `--\x3e'\n(let-env (scheme-report-environment 7)\n   (write (vector-map + #(1 2 3) #(4 5 6 7))))\n;; ==> #(5 7 9)\n(let-env (scheme-report-environment 5)\n   (write (vector-map + #(1 2 3) #(4 5 6 7))))\n;; ==> Unbound variable `vector-map'\n"})}),"\n",(0,i.jsxs)(n.p,{children:["R",(0,i.jsx)("sup",{children:"5"}),"RS doesn't support ",(0,i.jsx)(n.code,{children:"vector-map"})," that was added in version R",(0,i.jsx)("sup",{children:"7"}),"RS. The same\nboth Scheme versions doesn't support LIPS extensions like ",(0,i.jsxs)(n.a,{href:"/docs/lips/intro#helper-macros-and-functions",children:[(0,i.jsx)(n.code,{children:"--\x3e"}),"\nmacro"]}),"."]}),"\n",(0,i.jsx)(n.h2,{id:"introspection",children:"Introspection"}),"\n",(0,i.jsxs)(n.p,{children:["Since environments are JavaScript objects you can access its properties like ",(0,i.jsx)(n.code,{children:"__name__"})," or ",(0,i.jsx)(n.code,{children:"__env__"}),"."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:'(let ((x 10) (y 20))\n  (write (Object.keys (. (current-environment) \'__env__))))\n;; ==> #("x" "y")\n'})}),"\n",(0,i.jsxs)(n.p,{children:[(0,i.jsx)(n.code,{children:"__env__"})," property is an object with the variables. Here it returns variables defined in let."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:'(let ((x 10) (y 20))\n  (let ((env (current-environment)))\n    (write env.__name__)))\n;; ==> "let"\n'})}),"\n",(0,i.jsx)(n.p,{children:"Here you can access name of the lexical environment."}),"\n",(0,i.jsx)(n.h2,{id:"frames",children:"Frames"}),"\n",(0,i.jsxs)(n.p,{children:["In LIPS inspired by ",(0,i.jsx)(n.a,{href:"http://adv-r.had.co.nz/Environments.html",children:"R programming language"}),", there are\ntwo procedures ",(0,i.jsx)(n.code,{children:"parent.frame"})," and ",(0,i.jsx)(n.code,{children:"parent.frames"})," you can use them to get access to function\ncall stack environments."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define (foo)\n  (define x 10)\n  (bar))\n\n(define (bar)\n  (define x 20)\n  (baz))\n\n(define (baz)\n   (for-each (lambda (env)\n                (let-env env\n                  (print x)))\n     ;; car is top level environment\n     (cdr (parent.frames))))\n(foo)\n;; ==> 10\n;; ==> 20\n"})}),"\n",(0,i.jsx)(n.p,{children:"You can mix lexical scope chain with frames:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:'(define (foo)\n  (define x 10)\n  (let ((y "world"))\n    (bar)))\n\n(define (bar)\n  (define x 20)\n  (let ((y "hello"))\n    (baz)))\n\n(define (baz)\n   (for-each (lambda (env)\n               (display env.__name__)\n               (display " ==> ")\n               (print (Object.keys env.__env__))\n               (display env.__parent__.__name__)\n               (display " ==> ")\n               (print (Object.keys env.__parent__.__env__)))\n     ;; car is top level environment\n     (cdr (parent.frames))))\n(foo)\n;; ==> let ==> #(y)\n;; ==> lambda ==> #(arguments parent.frame x)\n;; ==> let ==> #(y)\n;; ==> lambda ==> #(arguments parent.frame x)\n'})}),"\n",(0,i.jsx)(n.h2,{id:"global-environment",children:"Global environment"}),"\n",(0,i.jsxs)(n.p,{children:["in ",(0,i.jsx)(n.code,{children:"lips.env"})," is user global environment but real global environment where all functions and macros that are\nlocated (it's also a place where names from bootstrapping are saved) is ",(0,i.jsx)(n.code,{children:"lips.env.__parent__"}),"."]}),"\n",(0,i.jsx)(n.p,{children:"If you want to really overwrite builtin function."}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(define-macro (define x)\n  `(print ,x))\n\n(define 10)\n;; ==> 10\n(unset! define)\n(define foo 10)\n(print foo)\n;; ==> 10\n"})}),"\n",(0,i.jsxs)(n.p,{children:["If you execute this code in REPL or in a script it will only add ",(0,i.jsx)(n.code,{children:"define"})," into ",(0,i.jsx)(n.code,{children:"interaction-environment"}),".\nIf you really want to overwrite ",(0,i.jsx)(n.code,{children:"define"})," you can (not that you should):"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:"(let-env lips.env.__parent__\n  (define-macro (define x)\n    `(print ,x)))\n\n(define 10)\n;; ==> 10\n(unset! define)\n(define foo 10)\n;; ==> Unbound variable `define'\n"})}),"\n",(0,i.jsxs)(n.p,{children:["The only time you may want to use ",(0,i.jsx)(n.code,{children:"lips.env.__parent__"})," when you bootstrap the LIPS Scheme system."]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-scheme",children:'(let-env lips.env.__parent__\n  (load "<path or URL>/dist/std.xcb"))\n'})})]})}function h(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}},8453:(e,n,r)=>{r.d(n,{R:()=>o,x:()=>c});var i=r(6540);const s={},t=i.createContext(s);function o(e){const n=i.useContext(t);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function c(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:o(e.components),i.createElement(t.Provider,{value:n},e.children)}}}]);