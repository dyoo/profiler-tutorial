#lang racket
(require profile)

(define (generate-data n)
    (cond
      [(= n 0)
       '((blah))]
      [(even? n)
       (cons '(blah)
             (generate-data (- n 1)))]
      [(odd? n)
       (list '() (list (generate-data (- n 1))))]))

(define my-flatten
  (parameterize ([current-namespace (make-base-namespace)]
                 [compile-context-preservation-enabled #t])
    (dynamic-require "my-flatten.rkt" 'my-flatten)))



(define my-data (generate-data 30000))
(profile (void (my-flatten my-data)))