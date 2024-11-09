---
slug: lips-history
title: LIPS Scheme History
authors: jcubic
image: /img/lips-history.png
tags: [lips, scheme, history]
---

This is the first article on LIPS blog. In this article I will write about the history of LIPS
Scheme interpreter.

<!--truncate-->

## What is Scheme?

Scheme is a dialect of the Lisp. The second oldest programming language still in use (after
Fortran).  Lisp and Scheme have specific syntax with prefix notation and where everything is a list
(at least historically).  It's also [Homoiconic](https://en.wikipedia.org/wiki/Homoiconicity), which
means that code and data have the same represantion. This allows to write programs that modify the
code like it was data.

## What is LIPS?

LIPS name is a recursive ancronym which stands for **"LIPS Is Pretty Simple"**. LIPS Scheme is
implementation of Scheme programming language in JavaScript. It adds a lot of stuff on top of Scheme
to make it more powerful and easier to interact with JavaScript.

## History of LIPS

It all started in February 2018 when I've written the first version of a Lisp interpreter. You can
still see the code on [CodePen](https://codepen.io/jcubic/pen/gvvzdp). Then I moved the [development
to GitHub](https://github.com/jcubic/lips) and named the project LIPS.  The first release (version
0.2.0) is marked as Mar 2018.

The reason why I created another lisp in JavaScript was because I wanted to have an Emacs in browser
that would have a real lisp inside. That's why LIPS had dynamic scope as an option. GNU Emacs use
Elisp that for a long time had dynamic scope. So I was planing to emulate that.

At the beginning it was Lisp based on Scheme, but at one point after version
[0.20.1 dated as Jul 1, 2020](https://github.com/jcubic/lips/releases/tag/0.20.1), I've started
adding features on devel branch and decided that I want a full Scheme implementation. But it turns out
that there were way too many breaking changes to release the next version. So I decided that I will
release it as 1.0-beta. Since then, LIPS keeps introducing new Beta versions. You can see the
[latest release on GitHub](https://github.com/jcubic/lips/releases).

## Future of LIPS

For the future plans I want in final version 1.0 are implementation of continutations and Tail Calls
(<abbr title="Tail Call Optimization">TCO</abbr>) and to be compatible (more or less) with
[R<sup>7</sup>RS specification](https://standards.scheme.org/). To see the progress, you can check
[1.0 Milestone on GitHub](https://github.com/jcubic/lips/issues?q=is%3Aopen+is%3Aissue+milestone%3A1.0).
