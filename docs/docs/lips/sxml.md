---
sidebar_position: 3
description: "SXML it's what JSX is for JavaScript"
---

# SXML (e.g. for React)

SXML is a way to define XML or HTML inside Scheme code. In LIPS Scheme, it works like for JSX for libraries
like [React](https://react.dev/) or [Preact](https://preactjs.com/).

By default, with JSX you define code like this:

```jsx
function MessageButton({ message }) {
    function clickHandler() {
       alert(message);
    }
    return (
        <button className="btn btn-primary" onClick={clickHandler}>
          Click me!
        </button>
    );
}
```

The code defines a simple component that use a button and onClick handler.

You can write the same code in LIPS as:

```scheme
(define (MessageButton props)
  (let ((click-handler (lambda ()
                         (alert props.message))))
    (sxml (button (@ (className "btn btn-primary")
                     (onClick click-handler))
                  "Click me!"))))
```

To create an instance of this component, you use:

```scheme
(MessageButton (@ (message "LIPS Scheme")))
```

The main element is `sxml` macro that do the same transoformation as [JSX
compiler](https://legacy.reactjs.org/docs/introducing-jsx.html) like [Babel](https://babeljs.io/) do.

## Inserting the Scheme code into SXML

By default, symbols in SXML are treated as tags. If you want to put code like with `{ }` in JSX, you
need to use `~` symbol in front of S-Expression:

```scheme
(let ((x 10))
  (sxml (ul (li ~x)
            (li ~(+ x 1))
            (li ~(+ x 2)))))
```

You can also array from escaped expression:

```scheme
(sxml (ul ~(list->array
            (map (lambda (x)
                   (sxml (li ~x)))
                 (range 10)))))
```

Remember to use `list->array` (or `list->vector`) if you process lists.

## Using SXML with React and Preact

To use SXML with React you need to specify the main function that is used to create tags in JSX.
In Preact is `preact.h` and in React it's `React.createElement`. Here is a required setup for them:

```scheme
(define createElement React.createElement)
(pragma->sxml createElement)
(define <> React.Fragment)
```

With Preact it will just this:

```scheme
(define h preact.h)
```

Because default `pragma->sxml` is `h`.

```scheme
(pragma->sxml h)
```

Similarly, if you want to use SXML and `sxml` macro in LIPS with other libraries that accept JSX, all
you have to do is run `pragma->sxml`. This is macro that define `sxml` macro with proper element
creation function.

Here are few example applications:
* [Preact](https://codepen.io/jcubic/pen/PojYxBP?editors=1000)
* [React](https://codepen.io/jcubic/pen/mdMBLwb?editors=1000)
