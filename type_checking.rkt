#lang racket/base

(require "kanren.rkt")

;;; Environment functions
(define (Γ-lookup Γ x τ)
  ;; The environment is modeled as a simple assoc list, mapping
  ;; variables to types. The lookup function works by relating a
  ;; variable and its environment with a type by first checking if the
  ;; first thing in the assoc list matches the variable, and if not,
  ;; proceeding recursively down the list.
  (disj (fresh (Γ*) (== Γ (cons (cons x τ) Γ*)))
        (fresh (Γ* x* τ*)
               (conj (== Γ (cons (cons x* τ*) Γ*))
                     (Γ-lookup Γ* x τ)))))

(define (Γ-extend Γ x τ Γ*)
  ;; This slaps a new pair on the front of the environment assoc list.
  (== (cons (cons x τ) Γ) Γ*))

;;; Type checker core
(define (type-for expr Γ t)
  (conde
   [(== (cons '? number?) expr) (== t 'number)]
   [(== (cons '? boolean?) expr) (== t 'boolean)]
   [(== (cons '? symbol?) expr) (Γ-lookup Γ expr t)]
   [(fresh (op)
           (== expr `(zero? ,op))
           (type-for op Γ 'number)
           (== t 'boolean))]
   [(fresh (c t-case f-case arm-type)
           (== expr `(if ,c ,t-case ,f-case))
           (type-for c Γ 'boolean)
           (type-for t-case Γ arm-type)
           (type-for f-case Γ arm-type)
           (== t arm-type))]
   [(fresh (op1 op2)
           (== expr `(+ ,op1 ,op2))
           (type-for op1 Γ 'number)
           (type-for op2 Γ 'number)
           (== t 'number))]
   [(fresh (arg body arg-type Γ* body-type)
           (conj+ (== `(lambda ,arg ,body) expr)
                  (Γ-extend Γ arg arg-type Γ*)
                  (type-for body Γ* body-type)
                  (== t (cons arg-type body-type))))]
   [(fresh (fexpr arg arg-type body-type)
           (conj+ (== expr `(,fexpr ,arg))
                  (type-for fexpr Γ (cons arg-type body-type))
                  (type-for arg Γ arg-type)
                  (== t body-type)))]))
