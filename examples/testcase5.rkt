#lang rosette

(require "../lang.rkt")
(require "../sketch.rkt")
(require "../synthesize.rkt")

(current-bitwidth #f)

;; ((-1 < v0) || (v0 < -10))

;; R0 : v0
;; R1 : -1
;; R2 : -10
;; R3 : R1 < R0
;; R4 : R0 < R2
;; R5 : R3 || R4

(define LHS (sketch (list (insn lt-idx 1 0 0)
                          (insn lt-idx 0 2 0)
                          (insn or-idx 3 4 0))
                    5
                    1
                    2))

(define RHS-sketch (get-symbolic-sketch 3 1 2))

(define x (get-sym-hld-int))
(define -one (hld-int #f #f -1))
(define -ten (hld-int #f #f -10))
(define c0 (get-sym-hld-int))
(define c1 (get-sym-hld-int))

(displayln "For LHS (-1 < v0) || (v0 < -10)")
(verify-bool-expr LHS x -one -ten)
(displayln "For LHS ! ( (-1 < v0) || (v0 < -10) )")
(verify-bool-expr-false LHS x -one -ten)

(displayln "For LHS (c0 < v0) || (v0 < c1)")
(verify-bool-expr LHS x c0 c1)
(displayln "For LHS ! (co < v0) || (v0 < c1)")
(verify-bool-expr-false LHS x c0 c1)

(synth-rewrite RHS-sketch LHS x c0 c1)