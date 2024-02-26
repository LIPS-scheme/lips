---
sidebar_position: 7
description: Powerful feature of Scheme that allow to add new control flows
---

# Continuations

## Early exit

Scheme doesn't have a return expression, but with continuations you can add one.

```scheme
(define (find item lst)
  (call/cc (lambda (return)
             (let loop ((lst lst))
                (if (null? lst)
                    (return #f)
                    (if (equal? item (car lst))
                        (return lst)
                        (loop (cdr lst))))))))
```
