#lang racket
(provide my-flatten)

(define (my-flatten x)
  (reverse
   (let loop ([x x]
              [acc '()])
     (cond
      [(pair? x)
       (loop (cdr x) (loop (car x) acc))]
      [(null? x)
       acc]
      [else
       (cons x acc)]))))