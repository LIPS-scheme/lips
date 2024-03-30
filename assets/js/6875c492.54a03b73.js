"use strict";(self.webpackChunknew_docs=self.webpackChunknew_docs||[]).push([[8610],{9703:(e,t,n)=>{n.d(t,{Z:()=>c});n(7294);var o=n(5999),s=n(2244),a=n(5893);function c(e){const{metadata:t}=e,{previousPage:n,nextPage:c}=t;return(0,a.jsxs)("nav",{className:"pagination-nav","aria-label":(0,o.I)({id:"theme.blog.paginator.navAriaLabel",message:"Blog list page navigation",description:"The ARIA label for the blog pagination"}),children:[n&&(0,a.jsx)(s.Z,{permalink:n,title:(0,a.jsx)(o.Z,{id:"theme.blog.paginator.newerEntries",description:"The label used to navigate to the newer blog posts page (previous page)",children:"Newer Entries"})}),c&&(0,a.jsx)(s.Z,{permalink:c,title:(0,a.jsx)(o.Z,{id:"theme.blog.paginator.olderEntries",description:"The label used to navigate to the older blog posts page (next page)",children:"Older Entries"}),isNext:!0})]})}},9985:(e,t,n)=>{n.d(t,{Z:()=>c});n(7294);var o=n(9460),s=n(3665),a=n(5893);function c(e){let{items:t,component:n=s.Z}=e;return(0,a.jsx)(a.Fragment,{children:t.map((e=>{let{content:t}=e;return(0,a.jsx)(o.n,{content:t,children:(0,a.jsx)(n,{children:(0,a.jsx)(t,{})})},t.metadata.permalink)}))})}},1714:(e,t,n)=>{n.r(t),n.d(t,{default:()=>k});n(7294);var o=n(512),s=n(5999),a=n(8824),c=n(8264),l=n(5281),i=n(3692),r=n(1460),d=n(9703),u=n(197),p=n(9985),g=n(2212),m=n(2503),h=n(5893);function b(e){const t=function(){const{selectMessage:e}=(0,a.c)();return t=>e(t,(0,s.I)({id:"theme.blog.post.plurals",description:'Pluralized label for "{count} posts". Use as much plural forms (separated by "|") as your language support (see https://www.unicode.org/cldr/cldr-aux/charts/34/supplemental/language_plural_rules.html)',message:"One post|{count} posts"},{count:t}))}();return(0,s.I)({id:"theme.blog.tagTitle",description:"The title of the page for a blog tag",message:'{nPosts} tagged with "{tagName}"'},{nPosts:t(e.count),tagName:e.label})}function x(e){let{tag:t}=e;const n=b(t);return(0,h.jsxs)(h.Fragment,{children:[(0,h.jsx)(c.d,{title:n}),(0,h.jsx)(u.Z,{tag:"blog_tags_posts"})]})}function j(e){let{tag:t,items:n,sidebar:o,listMetadata:a}=e;const c=b(t);return(0,h.jsxs)(r.Z,{sidebar:o,children:[t.unlisted&&(0,h.jsx)(g.Z,{}),(0,h.jsxs)("header",{className:"margin-bottom--xl",children:[(0,h.jsx)(m.Z,{as:"h1",children:c}),(0,h.jsx)(i.Z,{href:t.allTagsPath,children:(0,h.jsx)(s.Z,{id:"theme.tags.tagsPageLink",description:"The label of the link targeting the tag list page",children:"View All Tags"})})]}),(0,h.jsx)(p.Z,{items:n}),(0,h.jsx)(d.Z,{metadata:a})]})}function k(e){return(0,h.jsxs)(c.FG,{className:(0,o.Z)(l.k.wrapper.blogPages,l.k.page.blogTagPostListPage),children:[(0,h.jsx)(x,{...e}),(0,h.jsx)(j,{...e})]})}},2212:(e,t,n)=>{n.d(t,{Z:()=>g});n(7294);var o=n(512),s=n(5999),a=n(5742),c=n(5893);function l(){return(0,c.jsx)(s.Z,{id:"theme.unlistedContent.title",description:"The unlisted content banner title",children:"Unlisted page"})}function i(){return(0,c.jsx)(s.Z,{id:"theme.unlistedContent.message",description:"The unlisted content banner message",children:"This page is unlisted. Search engines will not index it, and only users having a direct link can access it."})}function r(){return(0,c.jsx)(a.Z,{children:(0,c.jsx)("meta",{name:"robots",content:"noindex, nofollow"})})}var d=n(5281),u=n(9047);function p(e){let{className:t}=e;return(0,c.jsx)(u.Z,{type:"caution",title:(0,c.jsx)(l,{}),className:(0,o.Z)(t,d.k.common.unlistedBanner),children:(0,c.jsx)(i,{})})}function g(e){return(0,c.jsxs)(c.Fragment,{children:[(0,c.jsx)(r,{}),(0,c.jsx)(p,{...e})]})}},3155:(e,t,n)=>{n.d(t,{Z:()=>_});var o=n(7294),s=n(2389),a=n(512),c=n(5281);const l={codeBlockContainer:"codeBlockContainer_APcc"};var i=n(5893);function r(e){let{as:t,...n}=e;return(0,i.jsx)(t,{...n,className:(0,a.Z)(n.className,l.codeBlockContainer,c.k.common.codeBlock)})}const d={codeBlockContent:"codeBlockContent_m3Ux",codeBlockTitle:"codeBlockTitle_P25_",codeBlock:"codeBlock_qGQc",codeBlockStandalone:"codeBlockStandalone_zC50",codeBlockLines:"codeBlockLines_p187",codeBlockLinesWithNumbering:"codeBlockLinesWithNumbering_OFgW",buttonGroup:"buttonGroup_6DOT"};function u(e){let{children:t,className:n}=e;return(0,i.jsx)(r,{as:"pre",tabIndex:0,className:(0,a.Z)(d.codeBlockStandalone,"thin-scrollbar",n),children:(0,i.jsx)("code",{className:d.codeBlockLines,children:t})})}var p=n(6668),g=n(6412),m=n(7016),h=n(5448),b=n(2573);const x={codeLine:"codeLine_iPqp",codeLineNumber:"codeLineNumber_F4P7",codeLineContent:"codeLineContent_pOih"};function j(e){let{line:t,classNames:n,showLineNumbers:o,getLineProps:s,getTokenProps:c}=e;1===t.length&&"\n"===t[0].content&&(t[0].content="");const l=s({line:t,className:(0,a.Z)(n,o&&x.codeLine)}),r=t.map(((e,t)=>(0,i.jsx)("span",{...c({token:e,key:t})},t)));return(0,i.jsxs)("span",{...l,children:[o?(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)("span",{className:x.codeLineNumber}),(0,i.jsx)("span",{className:x.codeLineContent,children:r})]}):r,"\n"]})}var k=n(195),B=n(5999),f=n(345),N=n(7666);const w={copyButtonCopied:"copyButtonCopied__QnY",copyButtonIcons:"copyButtonIcons_FhaS",copyButtonIcon:"copyButtonIcon_phi_",copyButtonSuccessIcon:"copyButtonSuccessIcon_FfTR"};function C(e){let{code:t,className:n}=e;const[s,c]=(0,o.useState)(!1),l=(0,o.useRef)(void 0),r=(0,o.useCallback)((()=>{(0,k.Z)(t),c(!0),l.current=window.setTimeout((()=>{c(!1)}),1e3)}),[t]);return(0,o.useEffect)((()=>()=>window.clearTimeout(l.current)),[]),(0,i.jsx)("button",{type:"button","aria-label":s?(0,B.I)({id:"theme.CodeBlock.copied",message:"Copied",description:"The copied button label on code blocks"}):(0,B.I)({id:"theme.CodeBlock.copyButtonAriaLabel",message:"Copy code to clipboard",description:"The ARIA label for copy code blocks button"}),title:(0,B.I)({id:"theme.CodeBlock.copy",message:"Copy",description:"The copy button label on code blocks"}),className:(0,a.Z)("clean-btn",n,w.copyButton,s&&w.copyButtonCopied),onClick:r,children:(0,i.jsxs)("span",{className:w.copyButtonIcons,"aria-hidden":"true",children:[(0,i.jsx)(f.Z,{className:w.copyButtonIcon}),(0,i.jsx)(N.Z,{className:w.copyButtonSuccessIcon})]})})}var Z=n(670);const y={wordWrapButtonIcon:"wordWrapButtonIcon_iowe",wordWrapButtonEnabled:"wordWrapButtonEnabled_gY8A"};function T(e){let{className:t,onClick:n,isEnabled:o}=e;const s=(0,B.I)({id:"theme.CodeBlock.wordWrapToggle",message:"Toggle word wrap",description:"The title attribute for toggle word wrapping button of code block lines"});return(0,i.jsx)("button",{type:"button",onClick:n,className:(0,a.Z)("clean-btn",t,o&&y.wordWrapButtonEnabled),"aria-label":s,title:s,children:(0,i.jsx)(Z.Z,{className:y.wordWrapButtonIcon,"aria-hidden":"true"})})}function L(e){let{children:t,className:n="",metastring:o,title:s,showLineNumbers:a,language:c}=e;const{prism:{defaultLanguage:l,magicComments:r}}=(0,p.L)(),u=function(e){return e?.toLowerCase()}(c??(0,m.Vo)(n)??l),x=(0,g.p)(),k=(0,h.F)(),B=(0,m.bc)(o)||s,{lineClassNames:f,code:N}=(0,m.nZ)(t,{metastring:o,language:u,magicComments:r}),w=a??(0,m.nt)(o);return(0,i.jsxs)("div",{children:[B&&(0,i.jsx)("div",{className:d.codeBlockTitle,children:B}),(0,i.jsxs)("div",{children:[(0,i.jsx)(b.y$,{theme:x,code:N,language:u??"text",children:e=>{let{className:t,style:o,tokens:s,getLineProps:a,getTokenProps:c}=e;return(0,i.jsx)("pre",{tabIndex:0,ref:k.codeBlockRef,children:(0,i.jsx)("code",{children:s.map(((e,t)=>{return(0,i.jsx)(j,{line:e,getLineProps:a,getTokenProps:n.match(/lips/)?(o=c,function(){const{style:e,...t}=o(...arguments);return t}):c,classNames:f[t],showLineNumbers:w},t);var o}))})})}}),(0,i.jsxs)("div",{className:d.buttonGroup,children:[(k.isEnabled||k.isCodeScrollable)&&(0,i.jsx)(T,{className:d.codeButton,onClick:()=>k.toggle(),isEnabled:k.isEnabled}),(0,i.jsx)(C,{className:d.codeButton,code:N})]})]})]})}function _(e){let{children:t,...n}=e;const a=(0,s.Z)(),c=function(e){return o.Children.toArray(e).some((e=>(0,o.isValidElement)(e)))?e:Array.isArray(e)?e.join(""):e}(t),l="string"==typeof c?L:u;return(0,i.jsx)(l,{...n,children:c},String(a))}}}]);