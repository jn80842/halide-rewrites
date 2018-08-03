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
  (if (= i2 0)
      indeterminate
      (hld-op euclidean-div i1 i2)))

(define (hld-mod i1 i2)
  (if (= i2 0)
      indeterminate
      (hld-op euclidean-mod i1 i2)))

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

;; operators are in ascending strength order
;; chose an arbitrary ordering for the overloaded operators
(define operator-list
  (list hld-add ;; 0
        hld-sub ;; 1
        hld-mod ;; 2
        hld-mul ;; 3
        hld-div ;; 4
        hld-min ;; 5
        hld-max ;; 6
        hld-eq-int ;; 7
        hld-eq-bool ;; 8
        hld-lt ;; 9
        hld-and ;; 10
        hld-or ;; 11
        hld-not ;; 12
        hld-select-int ;; 13
        hld-select-bool)) ;; 14

(define (get-arity-by-idx idx)
  (list-ref (list 2 ;; add
                  2 ;; sub
                  2 ;; mod
                  2 ;; mul
                  2 ;; div
                  2 ;; min
                  2 ;; max
                  2 ;; eq int
                  2 ;; eq bool
                  2 ;; lt
                  2 ;; and
                  2 ;; or
                  1 ;; not
                  3 ;; select int
                  3) idx))