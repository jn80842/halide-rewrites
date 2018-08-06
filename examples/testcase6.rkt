#lang rosette

(require "../lang.rkt")
(require "../sketch.rkt")
(require "../synthesize.rkt")

(current-bitwidth #f)

;; -1 < x || (x + y) < y

;; R0 : x
;; R1 : y
;; R2 : -1
;; R3 : R2 < R0
;; R4 : R0 + R1
;; R5 : R4 < R1
;; R6 : R3 || R5

(define LHS (sketch (list (insn lt-idx 2 0 0)
                          (insn add-idx 0 1 0)
                          (insn lt-idx 4 1 0)
                          (insn or-idx 3 5 0))
                    6
                    2
                    1))

(define RHS-sketch (get-symbolic-sketch 4 2 1))

(define x (get-sym-hld-int))
(define y (get-sym-hld-int))
(define -one (hld-int #f #f -1))
(define c0 (get-sym-hld-int))

(verify-bool-expr LHS x y -one)

(verify-bool-expr LHS x y c0)

(synth-rewrite RHS-sketch LHS x y -one)