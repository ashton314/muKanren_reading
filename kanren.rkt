#lang racket/base

(struct var (counter) #:transparent)
(define var=? equal?)

;;                    substitution assoc list: variable → var-or-value
;;                    |    fresh variable counter
;;                    ↓    ↓
(define empty-state '(() . 0))

;; find a variable's value in the substitution
(define (walk varname subst)
  (let ([var-or-val (and (var? varname)
                         (assp (λ (v) (var=? varname v)) subst))])
    (if var-or-val                      ; Was it a variable we got, and did it have a
        (walk (cdr var-or-val) subst)   ; reference? If yes, follow the new variable.
        varname)))                      ; Otherwise, we have a value OR an unbound var.

;; Needed because Racket doesn't implement R6RS's assp function
;; natively afaik
(define (assp ? lst)
  (if (null? lst)
      #f
      (if (? (caar lst)) (car lst) (assp ? (cdr lst)))))

(define (extend-subst var val subst)
  (cons (cons var val) subst))

;;; Goal constructors

(define (== u v)
  (λ (subst/counter)
    (let ([newsubst (unify u v (car subst/counter))])
      (if newsubst
          ;; If we did get a new subst list, keep the old counter, but
          ;; put this new subst set onto the state.
          (unit (cons newsubst (cdr subst/counter)))
          mzero))))

;; Ah, yes, a monad
(define ≡ ==)
(define (unit subst/counter) (cons subst/counter mzero))
(define mzero '())

;; unify :: term → term → subst → subst | #f
(define (unify u v subst)
  ;; step 1: walk down the substitution reference chain for u and v
  (let ([u (walk u subst)]
        [v (walk v subst)])
    ;; step 2: attempt to unify
    (cond
      ;; succeed unchanged: both u and v refer to the same unbound var
      [(and (var? u) (var? v) (var=? u v))
       subst]
      ;; augment: *one* of u, v is var; bind to other
      [(var? u)
       (extend-subst u v subst)]
      [(var? v)
       (extend-subst v u subst)]
      ;; unify structures: walk down lists
      [(and (pair? u) (pair? v))
       (let ([subst₁ (unify (car u) (car v) subst)])
         (and subst₁ (unify (cdr u) (cdr v) subst₁)))]
      ;; last-ditch: are these two things equivalent?
      [else (and (equal? u v) subst)])))

;; goal :: state → state*
;; call/fresh :: (var → goal) → goal
(define (call/fresh fn)
  (λ (subst/counter)
    (let ([counter (cdr subst/counter)])
      ((fn (var counter)) (cons (car subst/counter) (+ counter 1))))))

(define (disj goal1 goal2)
  (λ (subst/counter)
    ;; add the results of two streams of states
    (mplus (goal1 subst/counter)
           (goal2 subst/counter))))

(define (conj goal1 goal2)
  (λ (subst/counter)
    ;; thread stream of states from running one goal through another
    (bind (goal1 subst/counter)
          goal2)))

;; mplus: like zip-list but for streams (which may be lazy)
(define (mplus stream1 stream2)
  (cond
    [(null? stream1) stream2]
    ;; handle the case where stream1 is a lazy stream; note how we
    ;; flip the order of the streams so we interleave them
    [(procedure? stream1) (λ () (mplus stream2 (stream1)))]
    [else (cons (car stream1) (mplus stream2 (cdr stream1)))]))

;; bind: like map for a stream of states with a goal
(define (bind $stream goal)
  (cond
    [(null? $stream) mzero]
    ;; force the thunk and run again
    [(procedure? $stream) (λ () (bind ($stream) goal))]
    ;; join the stream of states from running the goal on the first
    ;; state in the input stream with the result of running on the
    ;; rest of the states in the input stream
    [else (mplus (goal (car $stream))
                 (bind (cdr $stream) goal))]))
