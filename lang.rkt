#lang rosette

;; Expr -> IntExpr | BoolExpr;
;; 
;; IntExpr -> IntExpr + IntExpr
;;         | IntExpr - IntExpr
;;         | IntExpr * IntExpr
;;         | IntExpr / IntExpr
;;         | IntExpr % IntExpr
;;         | max(IntExpr, IntExpr)
;;         | min(IntExpr, IntExpr)
;;         | select(BoolExpr, IntExpr, IntExpr)
;;         | ramp(IntExpr, IntExpr)
;;         | broadcast(IntExpr)
;;         | WildInt
;;         | Int;
;; 
;; BoolExpr -> ! BoolExpr
;;         | BoolExpr && BoolExpr
;;         | BoolExpr || BoolExpr
;;         | IntExpr < IntExpr
;;         | IntExpr - Intexpr == 0
;;         | BoolExpr == BoolExpr
;;         | select(BoolExpr, BoolExpr, BoolExpr)
;;         | broadcast(BoolExpr)
;;         | WildBool
;;         | Bool;

(provide (all-defined-out))

;; operators are in ascending strength order
;; chose an arbitrary ordering for the overloaded operators
(define operator-list
  (list hld-add
        hld-sub
        hld-mod
        hld-mul
        hld-div
        hld-min
        hld-max
        hld-eq-int
        hld-eq-bool
        hld-lt
        hld-and
        hld-or
        hld-not
        hld-select-int
        hld-select-bool))

(struct hld-int (indet const val) #:transparent)

(define indeterminate (hld-int #t #f 0))

(define (hld-op op i1 i2)
  (if (or (hld-int-indet i1) (hld-int-indet i2))
      indeterminate
      (hld-int #f #f (op (hld-int-val i1) (hld-int-val i2)))))

(define (hld-add i1 i2)
  (hld-op + i1 i2))

(define (hld-sub i1 i2)
  (hld-op - i1 i2))

(define (hld-mul i1 i2)
  (hld-op * i1 i2))

;; note that this is not euclidean!
(define (hld-div i1 i2)
  (if (= i2 0)
      indeterminate
      (hld-op quotient i1 i2)))

;; note that this is not euclidean!
(define (hld-mod i1 i2)
  (if (= i2 0)
      indeterminate
      (hld-op modulo i1 i2)))

(define (hld-max i1 i2)
  (hld-op max i1 i2))

(define (hld-min i1 i2)
  (hld-op min i1 i2))

(define (hld-select-int b1 i1 i2)
  (if b1 i1 i2))

(define (hld-select-bool b1 b2 b3)
  (if b1 b2 b3))

;; ramp and broadcast

(define (hld-not b)
  (not b))

(define (hld-and b1 b2)
  (and b1 b2))

(define (hld-or b1 b2)
  (or b1 b2))

(define (hld-lt i1 i2)
  (if (or (hld-int-indet i1) (hld-int-indet i2))
      indeterminate
      (< (hld-int-val i1) (hld-int-val i2))))

(define (hld-eq-int i1 i2)
  (if (or (hld-int-indet i1) (hld-int-indet i2))
      indeterminate
      (= i1 i2)))

(define (hld-eq-bool b1 b2)
  (or (and b1 b2) (nor b1 b2)))

;; fold semantically does nothing, but is important for ordering
(define (hld-fold i1)
  i1)