#lang rosette

(require "../lang.rkt")
(require "../sketch.rkt")
(require "../synthesize.rkt")

(current-bitwidth #f)

;; test case: reduce (0 - x) / 2 <= (1 - x) / 2
;; ! ( (1 - x) / 2 > (0 - x) / 2 )

;; R0 : x
;; R1 : 0
;; R2 : 1
;; R3 : 2
;; R4 : R1 - R0
;; R5 : R4 / R3
;; R6 : R2 - R0
;; R7 : R6 / R3
;; R8 : R7 < R5
;; R9 : ! R8

(define LHS (sketch (list (insn 1 1 0 0)
                          (insn 4 4 3 0)
                          (insn 1 2 0 0)
                          (insn 4 6 3 0)
                          (insn 9 7 5 0)
                          (insn 12 8 0 0))
                    9
                    1
                    3))

(define RHS-sketch (get-symbolic-sketch 6 1 3))

(define x (get-sym-hld-int))
;(define zero (hld-int #f #f 0))
(define zero (get-sym-hld-int))
;(define one (hld-int #f #f 1))
(define one (get-sym-hld-int))
;(define two (hld-int #f #f 2))
(define two (get-sym-hld-int))

(synth-rewrite RHS-sketch LHS x zero one two)
