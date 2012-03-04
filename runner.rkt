#lang racket
(provide run)

(require "my-flatten.rkt")

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
   (printf "Current continuation-mark-set->context: ~s\n"
           (continuation-mark-set->context (current-continuation-marks)))
   (my-flatten my-data)))
