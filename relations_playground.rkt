#lang racket/base

(require "kanren.rkt")

(define (parent c p)
  (conde
   [(== c 'teddy) (== p 'saraih)]
   [(== c 'andrew) (== p 'steve)]
   [(== c 'brook) (== p 'steve)]
   [(== c 'caroline) (== p 'steve)]
   [(== c 'peter) (== p 'steve)]
   [(== c 'steve) (== p 'bill)]
   [(== c 'roger) (== p 'bill)]
   [(== c 'will) (== p 'roger)]
   [(== c 'andy) (== p 'roger)]
   [(== c 'earnest) (== p 'roger)]
   [(== c 'jack) (== p 'bill)]
   [(== c 'anne) (== p 'john)]
   [(== c 'danni) (== p 'john)]
   [(fresh (s) (married p s) (parent c s))]
   [(fresh (s) (married s p) (parent c s))]))

(define (married h w)
  (conde
   [(== h 'steve) (== w 'anne)]
   [(== h 'nate) (== w 'danni)]
   [(== h 'bill) (== w 'katie)]
   [(== h 'john) (== w 'kitty)]
   [(== h 'andrew) (== w 'saraih)]))

(define (grandparent g s)
  (fresh (p) (parent g p) (parent p s)))

(define (cousin c1 c2)
  (fresh (gp)
         (grandparent c1 gp)
         (grandparent c2 gp)))

;; (run 20 (rel p c) (conj+ (grandparent c p) (== (cons c p) rel)))
;; (list->set (run 10 (gp) (grandparent 'andrew gp)))

;; unfortunately we need to gather 93 results until we get all the
;; answers for bill's grandchildren:
;; (list->set (run 93 (c) (grandparent c 'bill)))

(define (fav-num n)
  (disj (== n 42)
        (== (cons '? even?) n)))
