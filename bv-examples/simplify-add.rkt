#lang rosette

(require "../bv-synthesize.rkt")

(current-bitwidth bvw)

;; rewrite(x + (y - x), y)

(define sk2-1 (get-fixed-symbolic-sketch2))

(define (original1 x y z w)
  (bvadd x (bvsub y x)))

(synthesize-rewrite2 original1 sk2-1 (for/list ([i (range 4)]) (get-sym-bv)) 3)

