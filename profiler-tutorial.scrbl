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
in Racket.


@section{Profiling a textbook @racket[append]}

Let's first motivate the example by considering a classical function:
how do we flatten a list full of atoms and other lists?  It's such a
common toy problem, especially since Racket already has @racket[flatten] in its
standard library.  But let's ignore that for the moment, and consider
the following re-implementation.

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

What's the hot-spot?  Let's pretend that we don't know already, and
see what @racketmodname[profile] can tell us.


@subsection{Running the profiler}

The profiler reports results at the granularity of defined functions,
and should help us isolate which particular function dominates the
runtime of our programs.

The simplest way to use the profiler is with @racket[profile], which
works similarly to @racket[time] in taking in an expression.

We need to make sure to run the profiler in as clean an environment as
possible.  That means outside DrRacket, unfortunately.  Also, to get
better results from the profile, we should turn off all inlining.  We
also have to turn off the JIT as well.

[... add details about run-profiler.rkt]


@verbatim|{
$ racket --no-jit run-profiler.rkt runner.rkt
Profiling results
-----------------
  Total cpu time observed: 5245ms (out of 6961ms)
  Number of samples taken: 41 (once every 128ms)

====================================================
                                Caller
Idx   Total        Self       Name+src        Local%
      ms(pct)      ms(pct)      Callee
====================================================
[1] 5245(100.0%)    0(0.0%)   run ...dyoo/work/profiler-tutorial/runner.rkt:18:0
                                my-flatten [2]100.0%
----------------------------------------------------
                                run [1]         0.0%
                                my-flatten [2]100.0%
[2] 5245(100.0%)    0(0.0%)   my-flatten ...profiler-tutorial/my-flatten.rkt:7:0
                                my-flatten [2]100.0%
                                my-append [3]   0.0%
----------------------------------------------------
                                my-flatten [2]100.0%
[3] 5245(100.0%) 5245(100.0%) my-append .../profiler-tutorial/my-flatten.rkt:4:0
----------------------------------------------------
}|

The profiler sees during the run of this program that there are three
functions in play, essentially.  It lists them in no particular order.
@;
@; ... it would be _really_ nice if it actually sorted them by Self ms.  I should ask why it's not.
@;

We should focus our attention on the third column, the @emph{Self ms},
which tells what percentage of time is actively being spent in a
function; these are the functions that are prime candidates for being the hotspot.

As we can see, the use of @racket[my-append] dominates the runtime of
this toy program.  Contrast this to the


@subsection{Aside: how do we fix this program?}

Glib answer: we shouldn't be reimplementing @racket[flatten] in the
first place.

Not-so-glib answer: we should avoid using @racket[append] here.  Given
that the cost of @racket[append] is linear in the length of its
inputs, and given that we're repeatedly calling @racket[append] across
the entirety of the list structure, we're got a complexity that's
quadratic.


If we re-arrange the computation with an accumulator, we can eliminate
all the superfluous @racket[append]s and regain linear-time bounds.
@codeblock|{
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
}|

It's a cautionary tale that the standard textbook homework solution
for @racket[append] is the Wrong Way to flatten lists.  We should be
wary of textbook solutions that haven't gone through the gaze of a
profiler.



@section{Pitfalls}

[... add details here of pitfalls about profiling...]

Don't run in DrRacket.

Be wary of the inliner/optimizer, which will hide things.





@section{A more realistic example...}
Ok, let's try using the profiler on a more realistic example.

@; I want to apply this on my ropes library, which I KNOW needs profiling help... :)
