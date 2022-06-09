
<a id="org9f35143"></a>

# Synopsis

I wanted to understand how [μKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf) works. This is an annotated journey through implementing the code from that paper.


<a id="org561e24a"></a>

# Description

μKanren is a very small implementation in the Kanren family: essentially these are little embedded Prolog implementations. μKanren is particularly interesting because its implementation is less than 40 lines of Scheme code, and makes no use of exotic language features. Indeed, if your language has closures, you can make yourself a μKanren.


<a id="org51eb467"></a>

# Implementing the Core


<a id="org3f579be"></a>

## Basic types

```
state :: subst × fresh_var_counter

subst :: assoc list mapping variable → variable | value

goal :: state → state*
```

Goals take a state and return a stream (lazy) of zero or more new states.

A state tells us a variable substitution that satisfies the constrains the goals created.


<a id="org607ebba"></a>

## The `walk` function

This takes a variable and a substitution list, and it will walk through the substitution list until it finds the ultimate reference of the variable given. Since variables can map to other variables in the substitution list (see the [3.1](#org3f579be) section) then `walk` traverses those transitive dependencies until it can&rsquo;t any more.

It *can* return another variable; if the last thing that one variable points to is another variable that is *not* present at the beginning of the list, then returning that variable is valid. This is important for the [unify](#org0585d3b) function.


<a id="org0585d3b"></a>

## Implementing `unify`

The [unify](kanren.rkt) function take two *things*, `u` and `v`, and tries to make them line up according to the substitution that you give as well.

```racket
(unify '(1 2 3) '(2 3 4) '())
#f

(unify '(1 2 3) '(1 2 3) '())
'()

(unify '(1 2 3) '(1 2 3) '(yay))
'(yay)

(unify (list 1 2 3) (list 1 (var 0) 3) '())
(list (cons (var 0) 2))

(unify (list 1 2 3) (list 1 (var 0) 3) `((,(var 0) 4)))
#f
```

This example illustrates how the `walk` function drills down:

```racket
(walk (var 0) `((,(var 0) . ,(var 1)) (,(var 1) . ,(var 2))))
(var 2)

(unify (list 1 2 3) (list 1 (var 0) 3) `((,(var 0) . ,(var 1)) (,(var 1) . 2)))
(list (cons (var 0) (var 1)) (cons (var 1) 2))
```

If we wanted to be able to unify more than just lists (e.g. rich structures) we would teach μKanren here in the `cond` how to walk those richer structures.

Successful unification returns the substitution list that made the two things unify. This is different from the passed-in substitution list when a variable is found to point to another variable.


<a id="org933d856"></a>

## Implementing `call/fresh`

The implementation of [call/fresh](kanren.rkt) depends on the structure of the state: in a pure language, we stick a fresh variable counter on the state so we can thread that fresh effect through the computation. I would like to try just using something like `gensym` for variable creation.


<a id="org11bdd95"></a>

## `AND` and `OR` goal constructors

The magic of the [disj](./kanren.rkt) and [conj](./kanren.rkt) functions is encapsulated in the `mplus` and `bind` functions.

With the `disj` function, we want to `OR` two goals, and the `conj` is to `AND` two goals. For `disj`, we run both goals and *add* them together. For `conj`, we run the first goal (seen by applying the first goal to the `subst/counter` variable) and then we thread the result of that to the second goal.

Exactly *how* we add the goal results together bzw. thread the state from one goal to another determines the properties of the search. I&rsquo;ll skip the detailed evolution of this (see the paper for a nice walk-through) but in the end we get lazily-evaluated interleaving stream handling so we can exhaust every finite stream.


<a id="org6c268c7"></a>

### What are streams?

Streams are lists of states with lazily-evaluated members.

Here&rsquo;s an example from the paper showing how streams need to be interleaved and be lazy:

```racket
(define (fives x)
  (disj (== x 5)
        (λ (s/c) (λ () ((fives x) s/c)))))

(define (sixes x)
  (disj (== x 6)
        (λ (s/c) (λ () ((sixes x) s/c)))))

(define fives-and-sixes (call/fresh (λ (x) (disj (fives x) (sixes x)))))
```


<a id="orgde6cebe"></a>

# Extensions

These are some syntactic sugar that make working with μKanren nicer. Most of them are macros, which would make porting these to other languages less straight-forward. But they do make working in Scheme/Racket a lot nicer. Some new non-Lisp languages like Elixir<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> feature hygienic macro systems, so these features would be portable.


<a id="org2e706dd"></a>

# Modifications


<a id="org9273982"></a>

## Variable representation

I deviated from the paper&rsquo;s implementation of variables and wrote them as structs instead of vectors. I think further changes could be made (e.g. not having to keep around a number in the state to generate fresh variable names but these might rely on some more language-specific features. (E.g. generating fresh strings/symbols.)


<a id="org871f9f9"></a>

## Predicates in `unify`

I&rsquo;ve added some rudimentary predicate checking to the `unify` function:

```racket
(define (fav-num n)
  (disj (== n 42)
        (== (cons '? even?) n)))
```

```racket
> (run* (n) (== n 12) (fav-num n))
'(12)

> (run* (n) (== n 13) (fav-num n))
'()

> (run* (n) (fav-num n))
'(42 (? . #<procedure:even?>))
```


<a id="org31513dc"></a>

# Applications


<a id="org97ffd5f"></a>

## Family tree relationships

The classic example. See <./relations_playground.rkt>. Because of how the relations are defined, this will print out an infinite list of relations if you try to run certain queries, so best use the `run` function with some finite (and preferably small number; it doesn&rsquo;t take much to cover the whole space at least once) bound, as opposed to just running `run*`.


<a id="orgcaa07ef"></a>

## Type checking

See <./type_checking.rkt> for an implementation of a simple type checker/inference algorithm. Here is how you check the type of a program:

```racket
> (run* (type) (type-for '((lambda x x) 2) '() type))
'(number)

> (run* (type) (type-for '((lambda x (zero? x)) 2) '() type))
'(boolean)

> (run* (type) (type-for '((lambda x (zero? x)) #f) '() type))
'()  ;; type error

> (run* (type) (type-for '(lambda x x) '() type))
'((_.5 . _.5)) ;; generic type: a -> a
```

Here&rsquo;s the crazy thing: you can actually ask for programs that match a given type, since relations work both ways. Here&rsquo;s an example of generating five programs that are of type `number → boolean`:

```racket
> (run 5 (prog) (type-for prog '() (cons 'number 'boolean)))
'((lambda _.1 (? . #<procedure:boolean?>))
  (lambda _.1 (zero? (? . #<procedure:number?>)))
  (lambda (? . #<procedure:symbol?>) (zero? (? . #<procedure:symbol?>)))
  (lambda _.1 (zero? (+ (? . #<procedure:number?>) (? . #<procedure:number?>))))
  (lambda _.1 (if (? . #<procedure:boolean?>) (? . #<procedure:boolean?>) (? . #<procedure:boolean?>))))
```


<a id="org56e8cbe"></a>

# Author

I hope is *very clear* that *I* did *not* write the μKanren paper. That would be Daniel P. Friedman and Jason Hemann. I merely wrote up this annotation.

Ashton Wiersdorf <ashton.wiersdorf@pobox.com>


<a id="org0d14ba5"></a>

# Further reading

Be sure to read [the actual paper](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf) which is freely available.

Other fun links:

-   [Unifying the Technical Interview](https://aphyr.com/posts/354-unifying-the-technical-interview)

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> Personally, I think of Elixir as a Lisp in Ruby&rsquo;s clothing running on the BEAM. But don&rsquo;t tell anyone that Lisp is quietly becoming the new hot thing in web development and some machine learning. 🤫
