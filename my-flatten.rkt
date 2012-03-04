#lang racket
(provide my-flatten)

(define (my-append x y)
  (append x y))

(define (my-flatten x)
  (cond
    [(pair? x)
     (my-append (my-flatten (car x))
                (my-flatten (cdr x)))]
    [(null? x)
     '()]
    [else
     (list x)]))