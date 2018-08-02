#lang rosette

(require "sketch.rkt")
(require "ordering.rkt")

(current-bitwidth #f)

(define (synth-RHS RHS-sketch LHS . inputs)
  (begin (clear-asserts!)
         (let ([evaled-LHS (apply (get-sketch-function LHS) inputs)]
               [evaled-RHS (apply (get-sketch-function RHS-sketch) inputs)])
           (begin
             (define binding (time (synthesize #:forall (harvest inputs)
                                               #:guarantee (assert (and (expr-greater-than LHS RHS-sketch)
                                                                        (equal? evaled-LHS evaled-RHS))))))
             (clear-asserts!)
             binding))))

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

(define synthed-sketch (sketch (list (insn 10 1 0 0)
                                     (insn 9 2 1 -16))
                               1
                               2
                               0))