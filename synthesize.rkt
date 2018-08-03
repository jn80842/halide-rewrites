#lang rosette

(require "sketch.rkt")
(require "ordering.rkt")

(provide (all-defined-out))

(define (synth-rewrite RHS-sketch LHS . inputs)
  (begin (clear-asserts!)
         (let ([evaled-LHS (apply (get-sketch-function LHS) inputs)]
               [evaled-RHS (apply (get-sketch-function RHS-sketch) inputs)])
           (begin
             (define binding (time (synthesize #:forall (harvest inputs)
                                               #:guarantee (assert (and (expr-greater-than LHS RHS-sketch)
                                                                        (equal? evaled-LHS evaled-RHS))))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "no solution found")
                 (displayln (print-sketch (evaluate RHS-sketch binding))))))))


