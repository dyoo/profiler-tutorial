#lang scribble/manual
@(require (for-label racket/base
                     profile)
          scribble/eval)

@(define my-eval (make-base-eval))

@title{Surveying your programs: using Racket's statistical profiler}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]

Sometimes one's programs don't run as quickly as we'd like.  What can we do to
chase down where the time is going?

This is a very brief introduction on how to use the 
@link["http://docs.racket-lang.org/profile/index.html"]{statistical profiler}
in Racket.  We'll motivate the example by considering a classical function: how do we flatten a list
full of atoms and other lists?  It's such a common problem that Racket already has @racket[flatten]
in its standard library.  But let's ignore that for the moment, and consider the following
re-implementation.

@codeblock|{
#lang racket
(provide my-flatten)

(define (my-append x y)
  (append x y))

(define (my-flatten x)
  (cond
    [(pair? x)
     (my-append (my-flatten (car x))
                (my-flatten (cdr x)))]
    [(null? x) '()]
    [else
     (list x)]))
            }|

For example:
@(my-eval '(require "my-flatten.rkt"))

@interaction[#:eval my-eval
                    (my-flatten '((1) ((2) (3) 4)))]

Simple enough.  What happens if we start passing it large s-expressions?  Let's write a
quick-and-dirty function called @racket[generate-data].
@interaction[#:eval my-eval
(define (generate-data n)
  (cond
    [(= n 0)
     '((blah))]
    [(even? n)
     (cons '(blah)
           (generate-data (- n 1)))]
    [(odd? n)
     (list '() (list (generate-data (- n 1))))]))
(my-flatten (generate-data 5))
                    ]

Blah blah blah.  Let's bump that number higher, and see how much time it takes.

@interaction[#:eval my-eval
                    (define my-data (generate-data 50000))
                    (define flattened (time (my-flatten my-data)))
                    (length flattened)]

At the time of this writing, @racket[my-flatten] takes more than ten seconds
on a fairly modern machine.
This seems unreasonably long, considering that the list we're getting back is so short.


What's the hot-spot?  The function's simple enough that we could probably figure this out by inspection, but let's
see what @racketmodname[profile] can tell us.
@interaction[#:eval my-eval
                    (require profile)]

The simplest way to use the profiler is with @racket[profile], which works similarly
to @racket[time] in taking in an expression.  The profiler's granularity is at the level of
defined functions.


Also, make sure to run the profiler in as clean an environment as possible.
That means outside DrRacket, unfortunately.  Also, to get better results from the profile, we
should turn off all inlining.