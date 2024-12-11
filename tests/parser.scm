(set-special! "<>" 'html lips.specials.SPLICE)

(define-macro (html . args)
  (let ((str (--> (list->array (map symbol->string args)) (join "+"))))
    `(string-append "<" ,str "/>")))

(define parser/t1 <>(foo bar))

(unset-special! "<>")

(set-special! "--" 'dash lips.specials.LITERAL)

(define-macro (dash x)
  `'(,(car x) . ,(cadr x)))

(define parser/t2 --(foo bar baz))

(unset-special! "--")

(define parser/t3 (. (lips.parse "(--)" (current-environment)) 0))

(set-special! ":" 'keyword lips.specials.LITERAL)

(define-macro (keyword n)
   `(string->symbol (string-append ":" (symbol->string ',n))))

(define parser/t4 :foo)

(set-special! ":" 'keyword lips.specials.SPLICE)

(define-macro (keyword . n)
   `(string->symbol (string-append ":" (symbol->string ',n))))

(define parser/t5 :foo)

(unset-special! ":")

(set-special! "::" 'cube)

(define (cube x)
  (if (number? x)
      (* x x x)
      `(let ((.x ,x))
         (* .x .x .x))))

(define parser/t6 (let ((x 3)) ::x))
(define parser/t7 (. (lips.parse "(let ((x 3)) ::x)" (current-environment)) 0))

(unset-special! "::")

(set-special! "#nil" 'nil-fn lips.specials.SYMBOL)
(define (nil-fn) '())

(define parser/t8 (list #nil #nil '#nil '#nil #nil))

(unset-special! "#nil")

(set-special! "$" 'raw-string lips.specials.SYMBOL)

(define (raw-string)
  (if (char=? (peek-char) #\")
      (begin
        (read-char)
        (let loop ((result (vector)) (char (peek-char)))
          (read-char)
          (if (char=? char #\")
              (apply string (vector->list result))
              (loop (vector-append result (vector char)) (peek-char)))))))

(define parser/t9 $"foo \ bar")

(unset-special! "$")

(set-special! "#:num" 'line-num lips.specials.SYMBOL)

(define (line-num)
  (let* ((lexer lips.__parser__.__lexer__)
         (token lexer.__token__))
    (+ token.col)))

(define parser/t10 (list #:num #:num #:num #:num))

(unset-special! "#:num")

(test "parser: #!fold-case"
      (lambda (t)
        (define foo 10)
        (t.is (to.throw FOO) #t)
        #!fold-case
        (t.is (to.throw FOO) #f)
        (define BAR 20)
        (t.is (* FOO Bar) 200)
        #!no-fold-case
        (t.is (to.throw FOO) #t)))

(test "parser: syntax extension"
      (lambda (t)
        (t.is parser/t1 "<foo+bar/>")
        (t.is parser/t2 '(foo . bar))
        (t.is parser/t3 '(--))
        (t.is parser/t4 ':foo)
        (t.is parser/t5 ':foo)
        (t.is parser/t6 27)
        (t.is parser/t7 '(let ((x 3)) (let ((.x x)) (* .x .x .x))))
        (t.is parser/t8 '(() () () () ()))
        (t.is parser/t9 "foo \\ bar")
        (t.snapshot parser/t10)))

(test "parser: escape hex literals"
      (lambda (t)
         (t.is (to.throw (. (lips.parse "\"\\x9\"") 0)) #t)
         (t.is "\uFFFF" "￿")
         (t.is "\x9;\x9;" "\t\t")
         (t.is (repr '|foo bar|) "foo bar")
         (t.is '|\x9;\x9;|  '|\t\t|)))

(test "parser: character literals"
      (lambda (t)
        (let ((a #\A) (b #\xFF))
          (t.is (and (string=? (type a) "character")
                     (string=? (type b) "character"))
                true)
          (t.is #\Space #\space)
          (t.is #\SPACE #\SPace)
          (t.is (a.valueOf) "A")
          (t.is (b.valueOf) "\xFF;"))))

(test "parser: quotes with literals"
      (lambda (t)
        (t.is ''#f '(quote #f))
        (t.is ''#x10 '(quote #x10))
        (t.is ''#o10 '(quote #o10))
        (t.is ''#b10 '(quote #b10))

        ;; binary
        (t.is ''#i#b10 '(quote #i#b10))
        (t.is ''#b#i10 '(quote #b#i10))
        (t.is ''#e#b10 '(quote #e#b10))
        (t.is ''#b#e10 '(quote #b#e10))

        ;; hex
        (t.is ''#i#x10A '(quote #i#x10A))
        (t.is ''#x#i10A '(quote #x#i10A))
        (t.is ''#e#x10A '(quote #e#x10A))
        (t.is ''#x#e10A '(quote #x#e10A))

        ;; octal
        (t.is ''#i#o10 '(quote #i#o10))
        (t.is ''#o#i10 '(quote #o#i10))
        (t.is ''#e#o10 '(quote #e#o10))
        (t.is ''#o#e10 '(quote #o#e10))))

(test "parser: it should ignore comments"
      (lambda (t)

        (t.is (list #;(foo bar (quux)) 10 20) (list 10 20))
        (t.is (list #;foo 10 20) (list 10 20))
        (t.is (list 10 #;10+10i 20) (list 10 20))
        (t.is (list #;#;42 10 10) (list 10))
        (t.is (list 10 ;foo bar
                    20)
              (list 10
                    20))))

(test "parser: it should return literal space"
      (lambda (t)
        (let ((str (make-string 10 #\ )))
          (t.is (string-length str) 10)
          (t.is (not (null? (--> str (match #/^\s{10}$/)))) #t))))

(test "parser: vector quoting"
      (lambda (t)
         (t.is `#(1 2 3) #(1 2 3))
         (t.is `#(1 2 foo) #(1 2 foo))
         (t.is '#(1 2 foo) #(1 2 foo))))


(test "parser: vector constants"
      (lambda (t)

        (define (v)
          #(1 2 3))

        (t.is (eq? (v) (v)) true)

        (define (v)
          `#(1 2 3))

        (t.is (eq? (v) (v)) true)))


(test "parser: escaping in strings"
      (lambda (t)
        ;; testing #48 - when writing code with string in Scheme
        ;; we need to double escape to get slash
        (define code (lips.parse "(--> \"<title>hello-world<\\/title>\"
                                       (match #/<title>([^<]+)<\\/title>/)
                                       1)"))
        (t.is (eval (. code 0)) "hello-world")))

(test "parser: processing strings"
      (lambda (t)
        (define list "\\" "\"" "\\\\" "\\\"")
        (t.is true true)))

(test "parser: datum labels"
      (lambda (t)
        (let ((x (list #0=(cons 1 2) #0#)))
          (set-car! (car x) 2)
          (t.is x '((2 . 2) (1 . 2))))

        (let ((x (list '#0=(1 2 3) '#0#)))
          (t.is (eq? (car x) (cadr x)) true))

        (let ((x (list #1='(1 2 3) #1#)))
          (t.is (eq? (car x) (cadr x)) true))

        (let ((x '(#2=(1 2 3) #2#)))
          (t.is (eq? (car x) (cadr x)) true))

        (let ((x '#3=(1 2 . #3#)))
          (t.is (eq? x (cddr x)) true))))

(test "parser: should throw an error on extra close paren"
      (lambda (t)
        (t.snapshot (try
               (lips.exec "(define x 10))")
               (catch (e)
                      e.message)))))

(test "parser: should process line after comment without text #260"
      (lambda (t)
        (t.plan 2);
        (t.is #t #t)
        (t.is #t #t)))

(test "parser: emoji character"
      (lambda (t)
        (let ((x #\💩))
          (t.is (--> x (valueOf) 'length) 2)
          (t.is (length (Array.from (x.valueOf))) 1))))

(test "parser: space character"
      (lambda (t)
        (let ((x #\ ))
          (t.is (x.valueOf) " "))))

(test "parser: newline character"
      (lambda (t)
        (let ((x #\
                 ))
          (t.is (x.valueOf) "\n"))))

(test "parser: should throw error on quote without expression"
      (lambda (t)
        (let ((specs '("(list ')" "(list '')")))
          (for-each (lambda (code)
                      (t.is (to.throw (lips.parse code)) #t))
                    specs))))

(test "parser: should throw an error on invalid dot sequennce #245"
      (lambda (t)
        (t.is (to.throw (lips.parse "(1 . 2 3)")) #t)))

(test "parser: should throw error on invalid hash token"
      (lambda (t)
        (t.is (to.throw (lips.parse "#f10")) #t)))

(test "parser: escape symbols"
      (lambda (t)
        (t.is (map symbol->string '(|name| name|| name|\|| name|\\|xxx name|\\\\| name|\\|))
              '("name" "name" "name|" "name\\xxx" "name\\\\" "name\\"))))

(test "lexer: should create tokens for simple list"
      (lambda (t)
        (t.is (lips.tokenize "(foo bar baz)")
              #("(" "foo" "bar" "baz" ")"))))

(test "lexer: should create tokens for numbers string and regexes"
      (lambda (t)
        (t.is (lips.tokenize "(foo #/( \\/)/g \"bar baz\" 10 1.1 10e2
                              10+10i +inf.0+inf.0i +nan.0+nan.0i 1/2+1/2i)")
              #("(" "foo" "#/( \\/)/g" "\"bar baz\"" "10" "1.1" "10e2"
                "10+10i" "+inf.0+inf.0i" "+nan.0+nan.0i" "1/2+1/2i" ")"))))

(test "lexer: should create token for alist"
      (lambda (t)
        (t.is (lips.tokenize "((foo . 10) (bar . 20) (baz . 30))")
              #("(" "(" "foo" "." "10" ")" "(" "bar" "." "20" ")" "("
                "baz" "." "30" ")" ")"))))

(test "lexer: should ignore comments"
      (lambda (t)
        (let ((code "(foo bar baz); (baz quux)"))
          (t.is (lips.tokenize code)
                #("(" "foo" "bar" "baz" ")")))))

(test "lexer: should handle semicolon in regexes and strings"
      (lambda (t)
        (let ((code "(\";()\" #/;;;/g baz); (baz quux)"))
          (t.is (lips.tokenize code)
                #("(" "\";()\"" "#/;;;/g" "baz" ")")))))

(test "lexer: with meta data"
      (lambda (t)
        (let* ((fs (require "fs"))
               (code (--> (fs.promises.readFile "./tests/stubs/macro.txt")
                          (toString))))
          (t.snapshot (lips.tokenize code true)))))

(test "lexer: should throw error on unterminated string"
      (lambda (t)
        (let ((code "\"foo"))
          (t.is (to.throw (lips.tokenize code)) #t))))

(test "parser: metadata"
      (lambda (t)
        (let* ((code "(define foo (lambda (x)
                                    (let ((y (* x x)))
                                      (+ x y))))")
               (env lips.env)
               (Parser lips.Parser)
               (parse lips.parse)
               (parser (new Parser (object :env env :meta true))))
          (parser.prepare code)
          (t.snapshot (parse parser)))))

(test "parser: lonely cosing paren"
      (lambda (t)
        (t.snapshot (try (let* ((code "    )")
                                (env lips.env)
                                (parser (new lips.Parser (object :env env :meta true))))
                           (parser.prepare code)
                           (lips.parse parser))
                         (catch (e) e)))))
