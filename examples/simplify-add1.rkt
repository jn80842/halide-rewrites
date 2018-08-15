#lang rosette

(require "../lang.rkt")
(require "../sketch.rkt")
(require "../synthesize.rkt")

(current-bitwidth #f)

;; rewrite(x + (y - x), y)

;; R0: x
;; R1: y
;; R2: R1 - R0
;; R3: R0 + R2

(define LHS (sketch (list (insn 1 1 0 0 2)
                          (insn 0 0 2 0 2))
                    3
                    2
                    0))

(define x (get-sym-hld-int))
(define y (get-sym-hld-int))

(define RHS-sketch (get-symbolic-sketch 2 2 1))

(synth-rewrite RHS-sketch LHS x y (hld-int #f #f 1))

;; (rewrite((x - y) + (y - z), x - z)

;; R0 : x
;; R1 : y
;; R2 : z
;; R3 : R0 - R1
;; R4 : R1 - R2
;; R5 : R3 + R4

(define LHS2 (sketch (list (insn sub-idx 0 1 0 2)
                          (insn sub-idx 1 2 0 2)
                          (insn add-idx 3 4 0 4))
                    5
                    3
                    0))

(define z (get-sym-hld-int))

(define RHS-sketch2 (get-symbolic-sketch 3 3 1))

(synth-rewrite RHS-sketch2 LHS2 x y z (hld-int #f #f 1))