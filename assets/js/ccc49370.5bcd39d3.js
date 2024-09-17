"use strict";(self.webpackChunknew_docs=self.webpackChunknew_docs||[]).push([[3249],{3858:(e,t,n)=>{n.r(t),n.d(t,{default:()=>j});n(6540);var o=n(4164),a=n(1213),i=n(7559),s=n(7131),c=n(6535),l=n(4651),r=n(1312),d=n(9022),u=n(4848);function m(e){const{nextItem:t,prevItem:n}=e;return(0,u.jsxs)("nav",{className:"pagination-nav docusaurus-mt-lg","aria-label":(0,r.T)({id:"theme.blog.post.paginator.navAriaLabel",message:"Blog post page navigation",description:"The ARIA label for the blog posts pagination"}),children:[n&&(0,u.jsx)(d.A,{...n,subLabel:(0,u.jsx)(r.A,{id:"theme.blog.post.paginator.newerPost",description:"The blog post button label to navigate to the newer/previous post",children:"Newer Post"})}),t&&(0,u.jsx)(d.A,{...t,subLabel:(0,u.jsx)(r.A,{id:"theme.blog.post.paginator.olderPost",description:"The blog post button label to navigate to the older/next post",children:"Older Post"}),isNext:!0})]})}function p(){const{assets:e,metadata:t}=(0,s.e)(),{title:n,description:o,date:i,tags:c,authors:l,frontMatter:r}=t,{keywords:d}=r,m=e.image??r.image;return(0,u.jsxs)(a.be,{title:n,description:o,keywords:d,image:m,children:[(0,u.jsx)("meta",{property:"og:type",content:"article"}),(0,u.jsx)("meta",{property:"article:published_time",content:i}),l.some((e=>e.url))&&(0,u.jsx)("meta",{property:"article:author",content:l.map((e=>e.url)).filter(Boolean).join(",")}),c.length>0&&(0,u.jsx)("meta",{property:"article:tag",content:c.map((e=>e.label)).join(",")})]})}var g=n(5260),h=n(6676);function f(){const e=(0,h.J)();return(0,u.jsx)(g.A,{children:(0,u.jsx)("script",{type:"application/ld+json",children:JSON.stringify(e)})})}var b=n(7763),x=n(996);function v(e){let{sidebar:t,children:n}=e;const{metadata:o,toc:a}=(0,s.e)(),{nextItem:i,prevItem:r,frontMatter:d,unlisted:p}=o,{hide_table_of_contents:g,toc_min_heading_level:h,toc_max_heading_level:f}=d;return(0,u.jsxs)(c.A,{sidebar:t,toc:!g&&a.length>0?(0,u.jsx)(b.A,{toc:a,minHeadingLevel:h,maxHeadingLevel:f}):void 0,children:[p&&(0,u.jsx)(x.A,{}),(0,u.jsx)(l.A,{children:n}),(i||r)&&(0,u.jsx)(m,{nextItem:i,prevItem:r})]})}function j(e){const t=e.content;return(0,u.jsx)(s.i,{content:e.content,isBlogPostPage:!0,children:(0,u.jsxs)(a.e3,{className:(0,o.A)(i.G.wrapper.blogPages,i.G.page.blogPostPage),children:[(0,u.jsx)(p,{}),(0,u.jsx)(f,{}),(0,u.jsx)(v,{sidebar:e.sidebar,children:(0,u.jsx)(t,{})})]})})}},7763:(e,t,n)=>{n.d(t,{A:()=>r});n(6540);var o=n(4164),a=n(5195);const i={tableOfContents:"tableOfContents_bqdL",docItemContainer:"docItemContainer_F8PC"};var s=n(4848);const c="table-of-contents__link toc-highlight",l="table-of-contents__link--active";function r(e){let{className:t,...n}=e;return(0,s.jsx)("div",{className:(0,o.A)(i.tableOfContents,"thin-scrollbar",t),children:(0,s.jsx)(a.A,{...n,linkClassName:c,linkActiveClassName:l})})}},5195:(e,t,n)=>{n.d(t,{A:()=>h});var o=n(6540),a=n(6342);function i(e){const t=e.map((e=>({...e,parentIndex:-1,children:[]}))),n=Array(7).fill(-1);t.forEach(((e,t)=>{const o=n.slice(2,e.level);e.parentIndex=Math.max(...o),n[e.level]=t}));const o=[];return t.forEach((e=>{const{parentIndex:n,...a}=e;n>=0?t[n].children.push(a):o.push(a)})),o}function s(e){let{toc:t,minHeadingLevel:n,maxHeadingLevel:o}=e;return t.flatMap((e=>{const t=s({toc:e.children,minHeadingLevel:n,maxHeadingLevel:o});return function(e){return e.level>=n&&e.level<=o}(e)?[{...e,children:t}]:t}))}function c(e){const t=e.getBoundingClientRect();return t.top===t.bottom?c(e.parentNode):t}function l(e,t){let{anchorTopOffset:n}=t;const o=e.find((e=>c(e).top>=n));if(o){return function(e){return e.top>0&&e.bottom<window.innerHeight/2}(c(o))?o:e[e.indexOf(o)-1]??null}return e[e.length-1]??null}function r(){const e=(0,o.useRef)(0),{navbar:{hideOnScroll:t}}=(0,a.p)();return(0,o.useEffect)((()=>{e.current=t?0:document.querySelector(".navbar").clientHeight}),[t]),e}function d(e){const t=(0,o.useRef)(void 0),n=r();(0,o.useEffect)((()=>{if(!e)return()=>{};const{linkClassName:o,linkActiveClassName:a,minHeadingLevel:i,maxHeadingLevel:s}=e;function c(){const e=function(e){return Array.from(document.getElementsByClassName(e))}(o),c=function(e){let{minHeadingLevel:t,maxHeadingLevel:n}=e;const o=[];for(let a=t;a<=n;a+=1)o.push(`h${a}.anchor`);return Array.from(document.querySelectorAll(o.join()))}({minHeadingLevel:i,maxHeadingLevel:s}),r=l(c,{anchorTopOffset:n.current}),d=e.find((e=>r&&r.id===function(e){return decodeURIComponent(e.href.substring(e.href.indexOf("#")+1))}(e)));e.forEach((e=>{!function(e,n){n?(t.current&&t.current!==e&&t.current.classList.remove(a),e.classList.add(a),t.current=e):e.classList.remove(a)}(e,e===d)}))}return document.addEventListener("scroll",c),document.addEventListener("resize",c),c(),()=>{document.removeEventListener("scroll",c),document.removeEventListener("resize",c)}}),[e,n])}var u=n(8774),m=n(4848);function p(e){let{toc:t,className:n,linkClassName:o,isChild:a}=e;return t.length?(0,m.jsx)("ul",{className:a?void 0:n,children:t.map((e=>(0,m.jsxs)("li",{children:[(0,m.jsx)(u.A,{to:`#${e.id}`,className:o??void 0,dangerouslySetInnerHTML:{__html:e.value}}),(0,m.jsx)(p,{isChild:!0,toc:e.children,className:n,linkClassName:o})]},e.id)))}):null}const g=o.memo(p);function h(e){let{toc:t,className:n="table-of-contents table-of-contents__left-border",linkClassName:c="table-of-contents__link",linkActiveClassName:l,minHeadingLevel:r,maxHeadingLevel:u,...p}=e;const h=(0,a.p)(),f=r??h.tableOfContents.minHeadingLevel,b=u??h.tableOfContents.maxHeadingLevel,x=function(e){let{toc:t,minHeadingLevel:n,maxHeadingLevel:a}=e;return(0,o.useMemo)((()=>s({toc:i(t),minHeadingLevel:n,maxHeadingLevel:a})),[t,n,a])}({toc:t,minHeadingLevel:f,maxHeadingLevel:b});return d((0,o.useMemo)((()=>{if(c&&l)return{linkClassName:c,linkActiveClassName:l,minHeadingLevel:f,maxHeadingLevel:b}}),[c,l,f,b])),(0,m.jsx)(g,{toc:x,className:n,linkClassName:c,...p})}},996:(e,t,n)=>{n.d(t,{A:()=>p});n(6540);var o=n(4164),a=n(1312),i=n(5260),s=n(4848);function c(){return(0,s.jsx)(a.A,{id:"theme.unlistedContent.title",description:"The unlisted content banner title",children:"Unlisted page"})}function l(){return(0,s.jsx)(a.A,{id:"theme.unlistedContent.message",description:"The unlisted content banner message",children:"This page is unlisted. Search engines will not index it, and only users having a direct link can access it."})}function r(){return(0,s.jsx)(i.A,{children:(0,s.jsx)("meta",{name:"robots",content:"noindex, nofollow"})})}var d=n(7559),u=n(7293);function m(e){let{className:t}=e;return(0,s.jsx)(u.A,{type:"caution",title:(0,s.jsx)(c,{}),className:(0,o.A)(t,d.G.common.unlistedBanner),children:(0,s.jsx)(l,{})})}function p(e){return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(r,{}),(0,s.jsx)(m,{...e})]})}},6676:(e,t,n)=>{n.d(t,{k:()=>d,J:()=>u});var o=n(6025),a=n(4586),i=n(6803);var s=n(7131);const c=e=>new Date(e).toISOString();function l(e){const t=e.map(m);return{author:1===t.length?t[0]:t}}function r(e,t,n){return e?{image:p({imageUrl:t(e,{absolute:!0}),caption:`title image for the blog post: ${n}`})}:{}}function d(e){const{siteConfig:t}=(0,a.A)(),{withBaseUrl:n}=(0,o.h)(),{metadata:{blogDescription:i,blogTitle:s,permalink:d}}=e,u=`${t.url}${d}`;return{"@context":"https://schema.org","@type":"Blog","@id":u,mainEntityOfPage:u,headline:s,description:i,blogPost:e.items.map((e=>function(e,t,n){const{assets:o,frontMatter:a,metadata:i}=e,{date:s,title:d,description:u,lastUpdatedAt:m}=i,p=o.image??a.image,g=a.keywords??[],h=`${t.url}${i.permalink}`,f=m?c(m):void 0;return{"@type":"BlogPosting","@id":h,mainEntityOfPage:h,url:h,headline:d,name:d,description:u,datePublished:s,...f?{dateModified:f}:{},...l(i.authors),...r(p,n,d),...g?{keywords:g}:{}}}(e.content,t,n)))}}function u(){const e=function(){const e=(0,i.A)(),t=e?.data?.blogMetadata;if(!t)throw new Error("useBlogMetadata() can't be called on the current route because the blog metadata could not be found in route context");return t}(),{assets:t,metadata:n}=(0,s.e)(),{siteConfig:d}=(0,a.A)(),{withBaseUrl:u}=(0,o.h)(),{date:m,title:p,description:g,frontMatter:h,lastUpdatedAt:f}=n,b=t.image??h.image,x=h.keywords??[],v=f?c(f):void 0,j=`${d.url}${n.permalink}`;return{"@context":"https://schema.org","@type":"BlogPosting","@id":j,mainEntityOfPage:j,url:j,headline:p,name:p,description:g,datePublished:m,...v?{dateModified:v}:{},...l(n.authors),...r(b,u,p),...x?{keywords:x}:{},isPartOf:{"@type":"Blog","@id":`${d.url}${e.blogBasePath}`,name:e.blogTitle}}}function m(e){return{"@type":"Person",...e.name?{name:e.name}:{},...e.title?{description:e.title}:{},...e.url?{url:e.url}:{},...e.email?{email:e.email}:{},...e.imageURL?{image:e.imageURL}:{}}}function p(e){let{imageUrl:t,caption:n}=e;return{"@type":"ImageObject","@id":t,url:t,contentUrl:t,caption:n}}},1202:(e,t,n)=>{n.d(t,{A:()=>w});var o=n(6540),a=n(2303),i=n(4164),s=n(7559);const c={codeBlockContainer:"codeBlockContainer_APcc"};var l=n(4848);function r(e){let{as:t,...n}=e;return(0,l.jsx)(t,{...n,className:(0,i.A)(n.className,c.codeBlockContainer,s.G.common.codeBlock)})}const d={codeBlockContent:"codeBlockContent_m3Ux",codeBlockTitle:"codeBlockTitle_P25_",codeBlock:"codeBlock_qGQc",codeBlockStandalone:"codeBlockStandalone_zC50",codeBlockLines:"codeBlockLines_p187",codeBlockLinesWithNumbering:"codeBlockLinesWithNumbering_OFgW",buttonGroup:"buttonGroup_6DOT"};function u(e){let{children:t,className:n}=e;return(0,l.jsx)(r,{as:"pre",tabIndex:0,className:(0,i.A)(d.codeBlockStandalone,"thin-scrollbar",n),children:(0,l.jsx)("code",{className:d.codeBlockLines,children:t})})}var m=n(6342),p=n(6058),g=n(4291),h=n(6591),f=n(1765);const b={codeLine:"codeLine_iPqp",codeLineNumber:"codeLineNumber_F4P7",codeLineContent:"codeLineContent_pOih"};function x(e){let{line:t,classNames:n,showLineNumbers:o,getLineProps:a,getTokenProps:s}=e;1===t.length&&"\n"===t[0].content&&(t[0].content="");const c=a({line:t,className:(0,i.A)(n,o&&b.codeLine)}),r=t.map(((e,t)=>(0,l.jsx)("span",{...s({token:e,key:t})},t)));return(0,l.jsxs)("span",{...c,children:[o?(0,l.jsxs)(l.Fragment,{children:[(0,l.jsx)("span",{className:b.codeLineNumber}),(0,l.jsx)("span",{className:b.codeLineContent,children:r})]}):r,"\n"]})}var v=n(6861),j=n(1312),k=n(1473),L=n(4115);const N={copyButtonCopied:"copyButtonCopied__QnY",copyButtonIcons:"copyButtonIcons_FhaS",copyButtonIcon:"copyButtonIcon_phi_",copyButtonSuccessIcon:"copyButtonSuccessIcon_FfTR"};function B(e){let{code:t,className:n}=e;const[a,s]=(0,o.useState)(!1),c=(0,o.useRef)(void 0),r=(0,o.useCallback)((()=>{(0,v.A)(t),s(!0),c.current=window.setTimeout((()=>{s(!1)}),1e3)}),[t]);return(0,o.useEffect)((()=>()=>window.clearTimeout(c.current)),[]),(0,l.jsx)("button",{type:"button","aria-label":a?(0,j.T)({id:"theme.CodeBlock.copied",message:"Copied",description:"The copied button label on code blocks"}):(0,j.T)({id:"theme.CodeBlock.copyButtonAriaLabel",message:"Copy code to clipboard",description:"The ARIA label for copy code blocks button"}),title:(0,j.T)({id:"theme.CodeBlock.copy",message:"Copy",description:"The copy button label on code blocks"}),className:(0,i.A)("clean-btn",n,N.copyButton,a&&N.copyButtonCopied),onClick:r,children:(0,l.jsxs)("span",{className:N.copyButtonIcons,"aria-hidden":"true",children:[(0,l.jsx)(k.A,{className:N.copyButtonIcon}),(0,l.jsx)(L.A,{className:N.copyButtonSuccessIcon})]})})}var C=n(5048);const y={wordWrapButtonIcon:"wordWrapButtonIcon_iowe",wordWrapButtonEnabled:"wordWrapButtonEnabled_gY8A"};function A(e){let{className:t,onClick:n,isEnabled:o}=e;const a=(0,j.T)({id:"theme.CodeBlock.wordWrapToggle",message:"Toggle word wrap",description:"The title attribute for toggle word wrapping button of code block lines"});return(0,l.jsx)("button",{type:"button",onClick:n,className:(0,i.A)("clean-btn",t,o&&y.wordWrapButtonEnabled),"aria-label":a,title:a,children:(0,l.jsx)(C.A,{className:y.wordWrapButtonIcon,"aria-hidden":"true"})})}function _(e){let{children:t,className:n="",metastring:o,title:a,showLineNumbers:i,language:s}=e;const{prism:{defaultLanguage:c,magicComments:r}}=(0,m.p)(),u=function(e){return e?.toLowerCase()}(s??(0,g.Op)(n)??c),b=(0,p.A)(),v=(0,h.f)(),j=(0,g.wt)(o)||a,{lineClassNames:k,code:L}=(0,g.Li)(t,{metastring:o,language:u,magicComments:r}),N=i??(0,g._u)(o);return(0,l.jsxs)("div",{children:[j&&(0,l.jsx)("div",{className:d.codeBlockTitle,children:j}),(0,l.jsxs)("div",{children:[(0,l.jsx)(f.f4,{theme:b,code:L,language:u??"text",children:e=>{let{className:t,style:o,tokens:a,getLineProps:i,getTokenProps:s}=e;return(0,l.jsx)("pre",{tabIndex:0,ref:v.codeBlockRef,children:(0,l.jsx)("code",{children:a.map(((e,t)=>{return(0,l.jsx)(x,{line:e,getLineProps:i,getTokenProps:n.match(/lips/)?(o=s,function(){const{style:e,...t}=o(...arguments);return t}):s,classNames:k[t],showLineNumbers:N},t);var o}))})})}}),(0,l.jsxs)("div",{className:d.buttonGroup,children:[(v.isEnabled||v.isCodeScrollable)&&(0,l.jsx)(A,{className:d.codeButton,onClick:()=>v.toggle(),isEnabled:v.isEnabled}),(0,l.jsx)(B,{className:d.codeButton,code:L})]})]})]})}function w(e){let{children:t,...n}=e;const i=(0,a.A)(),s=function(e){return o.Children.toArray(e).some((e=>(0,o.isValidElement)(e)))?e:Array.isArray(e)?e.join(""):e}(t),c="string"==typeof s?_:u;return(0,l.jsx)(c,{...n,children:s},String(i))}}}]);