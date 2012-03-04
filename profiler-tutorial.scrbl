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


What's the hot-spot?  The function's simple enough that we could
probably figure this out by inspection, but let's see what
@racketmodname[profile] can tell us.  The simplest way to use the
profiler is with @racket[profile], which works similarly to
@racket[time] in taking in an expression.



@section{Unsuccessful profiling}


@section{Successful profiling}

We need to make sure to run the profiler in as clean an environment as
possible.  That means outside DrRacket, unfortunately.  Also, to get
better results from the profile, we should turn off all inlining.  We
also have to turn off the JIT as well.


@verbatim|{
$ racket --no-jit using-profile.rkt
Profiling results
-----------------
  Total cpu time observed: 5520ms (out of 7224ms)
  Number of samples taken: 41 (once every 135ms)

===========================================================
                                Caller
Idx   Total        Self       Name+src               Local%
      ms(pct)      ms(pct)      Callee
===========================================================
[1] 5520(100.0%)    0(0.0%)   profile-thunk12 ...t/collects/profile/main.rkt:9:0
                                call-handled-body [2]100.0%
-----------------------------------------------------------
                                profile-thunk12 [1]  100.0%
[2] 5520(100.0%)    0(0.0%)   call-handled-body ...private/more-scheme.rkt:209:2
                                run [3]              100.0%
-----------------------------------------------------------
                                call-handled-body [2]100.0%
[3] 5520(100.0%)    0(0.0%)   run ...1/lib/racket/collects/profile/main.rkt:29:2
                                temp1 [4]            100.0%
-----------------------------------------------------------
                                run [3]              100.0%
[4] 5520(100.0%)    0(0.0%)   temp1 (unknown source)
                                my-flatten [5]       100.0%
-----------------------------------------------------------
                                temp1 [4]              0.0%
                                my-flatten [5]       100.0%
[5] 5520(100.0%)    0(0.0%)   my-flatten ...profiler-tutorial/my-flatten.rkt:7:0
                                my-flatten [5]       100.0%
                                my-append [6]          0.0%
-----------------------------------------------------------
                                my-flatten [5]       100.0%
[6] 5520(100.0%) 5520(100.0%) my-append .../profiler-tutorial/my-flatten.rkt:4:0
}|


The profiler reports results at the granularity of defined functions.