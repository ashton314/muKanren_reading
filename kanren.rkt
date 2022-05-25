#lang racket/base

(provide (all-defined-out))

(struct var (name) #:transparent)
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

(define (occurs-check x v subst)
  (let ([v (walk v subst)])
    (if (var? v)
        (var=? v x)
        (and (pair? v) (or (occurs-check x (car v) subst)
                           (occurs-check x (cdr v) subst))))))

(define (extend-subst var val subst)
  (if (occurs-check var val subst)
      #f
      (cons (cons var val) subst)))

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
      ;; predicates
      [(and (pair? u) (eq? (car u) '?))
       (and ((cdr u) v) subst)]
      [(and (pair? v) (eq? (car v) '?))
       (and ((cdr v) u) subst)]
      ;; unify structures: walk down lists
      [(and (pair? u) (pair? v))
       (let ([subst₁ (unify (car u) (car v) subst)])
         (and subst₁ (unify (cdr u) (cdr v) subst₁)))]
      ;; last-ditch: are these two things equivalent?
      [else (and (eqv? u v) subst)])))

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

;;; Extentions

(define-syntax Zzz
  (syntax-rules ()
    ;; This is the inverse-η-delay abstracted in a macro
    [(_ goal) (λ (subst/counter) (λ () (goal subst/counter)))]))

(define-syntax conj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 gs ...) (conj (Zzz g0) (conj+ gs ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 gs ...) (disj (Zzz g0) (disj+ gs ...))]))

(define-syntax conde
  ;; call like
  ;;
  ;; (define (father p s)
  ;;   (conde ((== p 'paul) (== s 'jason))
  ;;          ((== p 'john) (== s 'henry))
  ;;          ((== p 'jason) (== s 'tom))
  ;;          ((== p 'peter) (== s 'brian))
  ;;          ((== p 'tom) (== s 'peter))))
  ;;
  ;; it is `or' applied across the subgroups of goals which are joined by `and'
  (syntax-rules ()
    [(_ (g gs ...) ...) (disj+ (conj+ g gs ...) ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g gs ...) (conj+ g gs ...)]
    [(_ (x xs ...) g gs ...)
     (call/fresh (λ (x) (fresh (xs ...) g gs ...)))]))

;;; Utilities to force streams
(define (pull $stream)
  (if (procedure? $stream) (pull ($stream)) $stream))

(define (take n $stream)
  (if (zero? n) '()
      (let ([$stream (pull $stream)])
        (cond
          [(null? $stream) '()]
          [else (cons (car $stream) (take (- n 1) (cdr $stream)))]))))

(define (take-all $stream)
  (let ([$stream (pull $stream)])
    (if (null? $stream) '() (cons (car $stream) (take-all (cdr $stream))))))

;;; Reification utilities
(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))

(define (reify-state/1st-var s/c)
  (let ([v (walk* (var 0) (car s/c))])
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (let ([n (reify-name (var-name v))])
         (cons (cons v n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(define (reify-name n)
  (if (symbol? n)
      n
      (string->symbol (string-append "_" "." (number->string n)))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [else v])))

(define (call/empty-state g) (g empty-state))

;;; Use these to wrap your queries: run takes a number of n solutions
;;; to the program, while run* calls `take-all'.
(define-syntax run
  (syntax-rules ()
    [(_ n (xs ...) g gs ...)
     (mK-reify (take n (call/empty-state
                        (fresh (xs ...) g gs ...))))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (xs ...) g gs ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (xs ...) g gs ...))))]))
