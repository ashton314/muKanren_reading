#lang racket/base

(require "kanren.rkt")

(define (parent c p)
  (conde
   [(== c 'teddy) (== p 'saraih)]
   [(== c 'andrew) (== p 'steve)]
   [(== c 'steve) (== p 'bill)]
   [(== c 'anne) (== p 'john)]
   [(fresh (s) (married p s) (parent c s))]
   [(fresh (s) (married s p) (parent c s))]))

(define (married h w)
  (conde
   [(== h 'steve) (== w 'anne)]
   [(== h 'bill) (== w 'katie)]
   [(== h 'john) (== w 'kitty)]
   [(== h 'andrew) (== w 'saraih)]))

(define (grandparent g s)
  (fresh (p) (parent g p) (parent p s)))

;; (run 20 (rel p c) (conj+ (grandparent c p) (== (cons c p) rel)))

(define (fav-num n)
  (disj (== n 42)
        (== (cons '? even?) n)))
