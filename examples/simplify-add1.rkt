#lang rosette

(require "../sketch.rkt")
(require "../synthesize.rkt")

(current-bitwidth #f)

;; rewrite(x + (y - x), y)

;; R0: x
;; R1: y
;; R2: R1 - R0
;; R3: R0 + R2

(define LHS (sketch (list (insn 1 1 0 0)
                          (insn 0 0 2 0))
                    3
                    2
                    0))

(define x (get-sym-hld-int))
(define y (get-sym-hld-int))

(define RHS-sketch (get-symbolic-sketch 2 2 0))

(synth-rewrite RHS-sketch LHS x y)