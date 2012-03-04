#lang racket
(provide run)

(require "my-flatten-2.rkt")

(define (generate-data n)
    (cond
      [(= n 0)
       '((blah))]
      [(even? n)
       (cons '(blah)
             (generate-data (- n 1)))]
      [(odd? n)
       (list '() (list (generate-data (- n 1))))]))

(define my-data (generate-data 30000))

(define (run)
  (void
   (my-flatten my-data)))
