#lang racket
(provide my-flatten)

(define (my-flatten x)
  (cond
    [(pair? x)
     (append (my-flatten (car x))
             (my-flatten (cdr x)))]
    [(null? x)
     '()]
    [else
     (list x)]))