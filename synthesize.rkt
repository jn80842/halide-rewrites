#lang rosette

(require "sketch.rkt")
(require "ordering.rkt")

(provide (all-defined-out))

(define (verify-bool-expr LHS . inputs)
  (begin (clear-asserts!)
         (let ([evaled-LHS (apply (get-sketch-function LHS) inputs)])
           (begin
             (define binding (time (verify (assert evaled-LHS))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "LHS is true")
                 (displayln "LHS is not provably true"))))))

(define (verify-bool-expr-false LHS . inputs)
  (begin (clear-asserts!)
         (let ([evaled-LHS (apply (get-sketch-function LHS) inputs)])
           (begin
             (define binding (time (verify (assert (not evaled-LHS)))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "LHS is true")
                 (displayln "LHS is not provably true"))))))

(define (synth-rewrite RHS-sketch LHS . inputs)
  (begin (clear-asserts!)
         (let ([evaled-LHS (apply (get-sketch-function LHS) inputs)]
               [evaled-RHS (apply (get-sketch-function RHS-sketch) inputs)])
           (begin
             (define binding (time (synthesize #:forall (symbolics inputs)
                                               #:guarantee (assert (and (expr-greater-than LHS RHS-sketch)
                                                                        (equal? evaled-LHS evaled-RHS))))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "no solution found")
                 (displayln (print-sketch (evaluate RHS-sketch binding))))))))


