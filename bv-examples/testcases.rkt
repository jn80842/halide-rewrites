#lang rosette

(require "../bv-synthesize.rkt")

(current-bitwidth bvw)

(define -one (bv -1 bvw))

(define sym-input2 (for/list ([i (range 4)]) (get-sym-bv)))

(define sk2 (get-fixed-symbolic-sketch2))

;; -1 < x || (x + y) < y

(define (lhs1 x y z w)
  (hld-or (bvslt z x) (bvslt (bvadd x y) y)))

;; need to assert inputs are restricted by size st. target function cannot over/underflow

(synthesize-rewrite2 lhs1 sk2 (list bx by -one bz) 3)