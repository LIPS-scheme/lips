"use strict";(self.webpackChunknew_docs=self.webpackChunknew_docs||[]).push([[6372],{2591:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>a,contentTitle:()=>l,default:()=>h,frontMatter:()=>c,metadata:()=>s,toc:()=>o});const s=JSON.parse('{"id":"lips/reflection","title":"Reflection","description":"A way to introspect and manipulate LIPS internals","source":"@site/docs/lips/reflection.md","sourceDirName":"lips","slug":"/lips/reflection","permalink":"/docs/lips/reflection","draft":false,"unlisted":false,"editUrl":"https://github.com/LIPS-scheme/lips/tree/master/docs/docs/lips/reflection.md","tags":[],"version":"current","sidebarPosition":2,"frontMatter":{"sidebar_position":2,"description":"A way to introspect and manipulate LIPS internals"},"sidebar":"tutorialSidebar","previous":{"title":"Core features","permalink":"/docs/lips/intro"},"next":{"title":"SXML (e.g. for React)","permalink":"/docs/lips/sxml"}}');var t=r(4848),i=r(8453);const c={sidebar_position:2,description:"A way to introspect and manipulate LIPS internals"},l="Reflection",a={},o=[{value:"Numbers",id:"numbers",level:2},{value:"Lists and Pairs",id:"lists-and-pairs",level:2},{value:"Strings",id:"strings",level:2},{value:"Characters",id:"characters",level:2},{value:"Procedures",id:"procedures",level:2}];function d(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,i.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.header,{children:(0,t.jsx)(n.h1,{id:"reflection",children:"Reflection"})}),"\n",(0,t.jsx)(n.p,{children:"You can use standard JavaScript methods to inspect LIPS objects."}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"Scheme functions"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"dir"})," - this procedure is inspired by Python function with the same name, it return all properties\nthat you can access on an object. Including those from object prototype and whole chain."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"env"})," - function return everything what is inside current-environment."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"JavaScript functions"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"Object.keys"})," - this JavaScript method will return all string keys from an object."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"Object.values"})," - return the value of the object."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"Object.entries"})," - this return array of ",(0,t.jsx)(n.code,{children:"[key,value]"})," pairs."]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.code,{children:"Object.getOwnPropertySymbols"})," - similar to ",(0,t.jsx)(n.code,{children:"Object.keys"})," but return all symbols"]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h2,{id:"numbers",children:"Numbers"}),"\n",(0,t.jsx)(n.p,{children:"You can access internal representation of numbers as JavaScript objects"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:"(let ((num 1/2+10i))\n  (print num.__im__)\n  (print num.__re__)\n  (print num.__re__.__denom__)\n  (print num.__re__.__num__))\n;; ==> 10\n;; ==> 1/2\n;; ==> 2\n;; ==> 1\n"})}),"\n",(0,t.jsx)(n.h2,{id:"lists-and-pairs",children:"Lists and Pairs"}),"\n",(0,t.jsx)(n.p,{children:"You can access Pairs as JavaScript objects:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:"(let ((x '(1 2 3)))\n  x.cdr.cdr.car)\n;; ==> 3\n"})}),"\n",(0,t.jsxs)(n.p,{children:["You can also manipulate the list with ",(0,t.jsx)(n.code,{children:"set!"}),":"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:"(let ((x '(1 2 3)))\n  (set! x.cdr.cdr.cdr x)\n  x)\n;; ==> #0=(1 2 3 . #0#)\n"})}),"\n",(0,t.jsx)(n.p,{children:"Above create a cycle. When you you try to display a cycle it's printed using R7RS datum syntax."}),"\n",(0,t.jsx)(n.h2,{id:"strings",children:"Strings"}),"\n",(0,t.jsx)(n.p,{children:"Same as with numbers and list you can access internals of Strings."}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:'(let ((str "hello"))\n  (str.__string__.toUpperCase)\n  (set! str.__string__ "world")\n  str)\n'})}),"\n",(0,t.jsxs)(n.p,{children:[(0,t.jsx)(n.code,{children:"__string__"})," property is read only so you can't modify it's value:"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:"(let ((str \"hello\"))\n  (set! str.__string__ \"world\")\n  str)\n;; ==> Cannot assign to read only property '__string__' of object '[object Object]'\n"})}),"\n",(0,t.jsx)(n.h2,{id:"characters",children:"Characters"}),"\n",(0,t.jsx)(n.p,{children:"Similar to string you can access internals of Characters."}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:"(let ((x #\\X)) (dir x))\n;; ==> (__char__ constructor toUpperCase toLowerCase toString serialize valueOf)\n"})}),"\n",(0,t.jsxs)(n.p,{children:["the ",(0,t.jsx)(n.code,{children:"__char__"})," property is a string that hold the value of the character."]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:'(let ((x #\\X))\n  (write x)\n  (newline)\n  (write x.__char__)\n  (newline))\n;; ==> #\\X\n;; ==> "X"\n'})}),"\n",(0,t.jsx)(n.h2,{id:"procedures",children:"Procedures"}),"\n",(0,t.jsxs)(n.p,{children:["Procedures as described in ",(0,t.jsx)(n.a,{href:"/docs/lips/intro#procedures",children:"Core features"})," are JavaScript functions,\nthey also hold additional properties like ",(0,t.jsx)(n.code,{children:"__code__"})," and ",(0,t.jsx)(n.code,{children:"__doc__"}),". The first property is the live\nsource code of the procedure that you can modify:"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:'(define (repeater x)\n   "(repeater value)\n\n    Function prints the value 1 time and modifies itself to repeat\n    (+ n 1) times on the next call."\n   (for-each (lambda () (print x)) (range 1))\n   (let ((r (cadr (cdadddr (. repeater \'__code__)))))\n     (set-cdr! r (list (+ (cadr r) 1)))))\n'})}),"\n",(0,t.jsx)(n.p,{children:"This procedure modify its source code. Each time you execute this function it will run one more\ntimes."}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:'(print "1")\n(repeater \'hello)\n;; ==> 1\n;; ==> hello\n(print "2")\n(repeater \'hello)\n;; ==> 2\n;; ==> hello\n;; ==> hello\n(print "3")\n(repeater \'hello)\n;; ==> 3\n;; ==> hello\n;; ==> hello\n;; ==> hello\n'})}),"\n",(0,t.jsxs)(n.p,{children:["The first expression of the procedure is a doc string, unless a string is the only expression,\nin that case it's a return value. To access doc string you can use ",(0,t.jsx)(n.code,{children:"help"})," or ",(0,t.jsx)(n.code,{children:"__doc__"}),"."]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-scheme",children:'repeater.__doc__\n"(repeater value)\n\nFunction prints the value 1 time and modifies itself to repeat\n(+ n 1) times on the next call."\n'})})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(d,{...e})}):d(e)}},8453:(e,n,r)=>{r.d(n,{R:()=>c,x:()=>l});var s=r(6540);const t={},i=s.createContext(t);function c(e){const n=s.useContext(i);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function l(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:c(e.components),s.createElement(i.Provider,{value:n},e.children)}}}]);