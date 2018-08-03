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

;; non-rewrite rule rewrites:
;; a <= b --> ! b > a
;; a > b  --> b > a
;; a >= b --> ! a < b
;; a != b --> ! a == b
;; a == b --> a - b == 0
;; - a    --> 0 - a

(provide (all-defined-out))

(define (div-in-Z-val x y)
  (if (= (modulo x y) 0) 0 1))

(define (euclidean-div x y)
  (cond [(and (negative? x) (negative? y)) (+ (- (quotient (abs x) y)) (div-in-Z-val x y))]
        [(negative? x) (- (- (quotient (abs x) y)) (div-in-Z-val x y)) ]
        [(negative? y) (quotient x y)]
        [else (quotient x y)]))

(define (euclidean-mod x y)
  (cond [(and (negative? x) (negative? y)) (- (- (modulo (abs x) (abs y))) (* y (div-in-Z-val x y)))]
        [(negative? x) (+ (- (modulo (abs x) y)) (* y (div-in-Z-val x y)))]
        [(negative? y) (modulo x (abs y))]
        [else (modulo x y)]))

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

(define (hld-div i1 i2)
  (if (= (hld-int-val i2) 0)
      indeterminate
      (hld-op euclidean-div i1 i2)))

(define (hld-mod i1 i2)
  (if (= (hld-int-val i2) 0)
      indeterminate
      (hld-op euclidean-mod i1 i2)))

(define (hld-max i1 i2)
  (hld-op max i1 i2))

(define (hld-min i1 i2)
  (hld-op min i1 i2))

(define (hld-select-int b1 i1 i2)
  (if (boolean? b1)
      (if b1 i1 i2)
      'failed-typecheck))

(define (hld-select-bool b1 b2 b3)
  (if (and (boolean? b1) (boolean? b2) (boolean? b3))
      (if b1 b2 b3)
      'failed-typecheck))

;; ramp and broadcast

(define (hld-not b)
  (if (boolean? b)
      (not b)
      'failed-typecheck))

(define (hld-and b1 b2)
  (if (and (boolean? b1) (boolean? b2))
      (and b1 b2)
      'failed-typecheck))

(define (hld-or b1 b2)
  (if (and (boolean? b1) (boolean? b2))
      (or b1 b2)
      'failed-typecheck))

(define (hld-lt i1 i2)
  (if (or (hld-int-indet i1) (hld-int-indet i2))
      indeterminate
      (< (hld-int-val i1) (hld-int-val i2))))

(define (hld-eq-int i1 i2)
  (if (or (hld-int-indet i1) (hld-int-indet i2))
      indeterminate
      (= i1 i2)))

(define (hld-eq-bool b1 b2)
  (if (and (boolean? b1) (boolean? b2))
      (or (and b1 b2) (nor b1 b2))
      'failed-typecheck))

;; fold semantically does nothing, but is important for ordering
(define (hld-fold i1)
  i1)

(struct operator (function arity name) #:transparent)

(define add-operator (operator hld-add 2 "hld-add"))
(define sub-operator (operator hld-sub 2 "hld-sub"))
(define mod-operator (operator hld-mod 2 "hld-mod"))
(define mul-operator (operator hld-mul 2 "hld-mul"))
(define div-operator (operator hld-div 2 "hld-div"))
(define min-operator (operator hld-min 2 "hld-min"))
(define max-operator (operator hld-max 2 "hld-max"))
(define eq-int-operator (operator hld-eq-int 2 "hld-eq-int"))
(define eq-bool-operator (operator hld-eq-bool 2 "hld-eq-bool"))
(define lt-operator (operator hld-lt 2 "hld-lt"))
(define and-operator (operator hld-and 2 "hld-and"))
(define or-operator (operator hld-or 2 "hld-or"))
(define not-operator (operator hld-not 1 "hld-not"))
(define select-int-operator (operator hld-select-int 3 "hld-select-int"))
(define select-bool-operator (operator hld-select-bool 3 "hld-select-bool"))

;; operators are in ascending strength order
;; chose an arbitrary ordering for the overloaded operators
(define operator-list
  (list add-operator ;; 0
        sub-operator ;; 1
        mod-operator ;; 2
        mul-operator ;; 3
        div-operator ;; 4
        min-operator ;; 5
        max-operator ;; 6
        eq-int-operator ;; 7
        eq-bool-operator ;; 8
        lt-operator ;; 9
        and-operator ;; 10
        or-operator ;; 11
        not-operator ;; 12
        select-int-operator ;; 13
        select-bool-operator)) ;; 14

(define (get-operator-by-idx idx)
  (list-ref operator-list idx))
(define (get-operator-arity-by-idx idx)
  (operator-arity (get-operator-by-idx idx)))
(define (get-operator-name-by-idx idx)
  (operator-name (get-operator-by-idx idx)))
(define (get-operator-function-by-idx idx)
  (operator-function (get-operator-by-idx idx)))