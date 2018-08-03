#lang rosette

(require "../sketch.rkt")
(require "../synthesize.rkt")

(current-bitwidth #f)

;; rewrite((select(x, y, z) + w) + select(x, u, v), select(x, y + u, z + v) + w)

;; R0 x (bool)
;; R1 y
;; R2 z
;; R3 w
;; R4 u
;; R5 v
;; R6 select R0 R1 R2
;; R7 R6 + R3
;; R8 select R0 R4 R5
;; R9 R7 + R8

(define select-LHS (sketch (list (insn 13 0 1 2)
                                 (insn 0 6 3 0)
                                 (insn 13 0 4 5)
                                 (insn 0 7 8 0))
                           9
                           6
                           0))

(define select-RHS (get-symbolic-sketch 4 6 0))

(define-symbolic* x boolean?)
(define y (get-sym-hld-int))
(define z (get-sym-hld-int))
(define w (get-sym-hld-int))
(define u (get-sym-hld-int))
(define v (get-sym-hld-int))

(synth-rewrite select-RHS select-LHS x y z w u v)