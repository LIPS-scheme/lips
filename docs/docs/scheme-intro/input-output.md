---
sidebar_position: 4
description: How to read/write files and how to interact with stdin/stdout
---

# Input and Output

## Standard output

The standard output is everything you print on the screen. Often [computer
terminal](https://en.wikipedia.org/wiki/Computer_terminal) or a [terminal
emulator](https://en.wikipedia.org/wiki/Terminal_emulator) that display so called
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

To print anything on the screen you use `(write x)` or `(display x)` and `(newline)` functions.

There is also a function that write single character `(write-char x)`

## Standard input

The standard input is a way to get stuff from the user. Scheme provide one function for this which
is `read`. The function wait for the user to type something and return single
[S-Expression](/docs/scheme-intro/what-is-lisp#s-expressions).

Example:

```scheme
(define list (read))
```

after this the Scheme interpreter will pause and wait for the input from user. If user type `(1 2
3)` (you don't need quote), and press enter, the read expression will return that list and Scheme
will define a variable named list and assign that list to that symbol.

### Reading single character
You also have a functions that operate on single characters:

* `(read-char)`
* `(peek-char)`

You can check if there are more characters in the stream by using `(eof-object? x)` predicate. `eof` is
special object that indicate the end of file.

### Reading single line
Scheme define function `(read-line)` that reads whole line.

## Ports
Ports are a thing in Scheme that allows to write into and read from. By default `read` read from
standard input, and `write`, `display`, and `newline` writes to standard output. But all those
functions accept an argument which allow to change the port to which it's writing or reading.

### Standard input port

To get standard input as port object you can use:

```scheme
(current-input-port)
;; ==> #<input-port>
```

The representation of the input port object can change depending on Scheme implementation.

You can use this port as optional argument in read:

```scheme
(define list (read (current-input-port)))
```

This will make the same effect as just `(read)`. But note that you can change current input port, by
different mechanism built into Scheme.

### Standard output port

To get standard output as object you can use:

```scheme
(current-output-port)
;; ==> #<output-port>
```

Same as with input port the representation of this object can be different depending on the Scheme implementation.

You can use this port with `write`, `display`, or `newline`:

```scheme
(define (print x)
  (let ((port (current-output-port)))
    (display x port)
    (newline port)))
```

If called at top level this function will have the same effect as:

```scheme
(define (print x)
  (display x)
  (newline))
```

Same as with input port you can change the current output port by different Scheme expressions.

## Predicates that test if object is port

You have function that check if something is input port:

* `(input-port? x)`

Or if something is output port:

* `(output-port? x)`

## Writing to a file

To write to a file first you need to have an open output port that points into a file. You can open file as output port with:

```scheme
(let ((port (open-output-file "test.scm")))
  (write "hello world" port)
  (newline port)
  (close-output-port port))
```

You need to remember to close the port to make sure that the data was properly written to the file. Some Scheme implementation
may decide to flush the buffer (write to a file) when you close the port.

You can use helper procedure that closes the port automatically:

```scheme
(call-with-output-file "test-2.scm"
  (lambda (port)
    (write "hello" port)
    (newline port)
    (write "there" port)
    (newline port)))
```

## Reading from a file

To read from a file you first need to open input port that points to a file. You can open input port with:

```scheme
(let ((port (open-input-file "test-2.scm")))
  (display (read port))
  (display " ")
  (display (read port))
  (newline)
  (close-input-port port))
;; ==> hello there
```

If you first evaluated the code that writes into a file `test-2.scm` you should see same string that
was written into it.

Same as with output-port you need to call `close-input-port` procedure to close the input port. But
Scheme, same as with output port have handy function that allow to close the port automagically.

```scheme
(call-with-input-file "test.scm"
  (lambda (port)
    (write (read port))
    (newline)))
;; ==> "hello world"
```

## Reading whole file

In specification there are no procedure that reads whole file as a string. But you can easily write such procedure.

```scheme
(define (read-file file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((char (read-char port)) (result '()))
        (if (eof-object? char)
            (apply string (reverse result))
            (loop (read-char port) (cons char result)))))))

(display (read-file "test-2.scm"))
;; ==> "hello"
;; ==> "there"
```

Same you can create a procedure that reads all lines from text as list of strings:

```scheme
(define (read-lines filename)
  (call-with-input-file filename
    (lambda (file)
      (let ((line ""))
        (let loop ((result '()))
          (set! line (read-line file))
          (if (eof-object? line)
              result
              (loop (append result (list line)))))))))

(display (read-lines "test-2.scm"))
;; ==> ("hello" "there")
```

## Writing lines to file
There are no procedure that write a string as a line into a port, but you can define one:

```scheme
(define (write-line str . rest)
  (let ((port (if (null? rest)
                  (current-output-port)
                  (car rest))))
    (display str port)
    (newline port)))
```

You can use this function to write list of strings as lines into a file:

```scheme
(define (write-lines lst . rest)
  (let loop ((lst lst))
    (unless (null? lst)
      (apply write-line (car lst) . rest)
      (loop (cdr lst)))))
```

You can simplify this function by using `for-each` higher order procedure.

```scheme
(define (write-lines lst . rest)
  (for-each (lambda (line) (apply write-line line rest)) lst))
```

You can use this with `call-with-output-file`:

```scheme
(call-with-output-file "test-3.scm"
  (lambda (port)
    (let ((beatles '("John Lennon"
                     "Paul McCartney"
                     "Ringo Starr"
                     "George Harrison")))
     (write-lines beatles port))))

(read-lines "test-3.scm")
;; ==> ("John Lennon" "Paul McCartney" "Ringo Starr" "George Harrison")
```
