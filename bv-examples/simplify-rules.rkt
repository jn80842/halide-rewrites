#lang rosette

(require "../bv-synthesize.rkt")

(current-bitwidth bvw)

(define-symbolic* bx (bitvector bvw))
(define-symbolic* by (bitvector bvw))
(define-symbolic* bz (bitvector bvw))
(define-symbolic* bw (bitvector bvw))
(define zero (bv 0 bvw))
(define one (bv 1 bvw))
(define two (bv 2 bvw))

(define sym-input2 (for/list ([i (range 4)]) (get-sym-bv)))

(define sk2 (get-fixed-symbolic-sketch2))

;; rewrite(x + (y - x), y)

(define (lhs1 x y z w)
  (bvadd x (bvsub y x)))

(synthesize-rewrite2 lhs1 sk2 sym-input2 3)

;; (rewrite((x - y) + (y - z), x - z)

(define (lhs2 x y z w)
  (bvadd (bvsub x y) (bvsub y z)))

(synthesize-rewrite2 lhs2 sk2 sym-input2 4)

;; rewrite(0 / x, 0)

(define (lhs3 x y z w) ;; assume x is 0
  (hld-div y x))

(synthesize-rewrite2 lhs3 sk2 (list (bv 0 bvw) (get-sym-bv) (get-sym-bv) (get-sym-bv)) 2)