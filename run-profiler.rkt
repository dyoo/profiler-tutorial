#lang racket/base

(define modname (vector-ref (current-command-line-arguments) 0))
(require syntax/strip-context)

(parameterize ([compile-context-preservation-enabled #t]
               [compile-enforce-module-constants #f]
               [eval-jit-enabled #f])
  (parameterize ([current-namespace (make-base-namespace)])
    (with-syntax ([modname modname])
      (eval (strip-context #'(module anonymous racket/base
                               (require profile)
                               (require modname)
                               (profile
                                (call-with-continuation-prompt run)))))
      (eval (strip-context #'(require 'anonymous))))))
