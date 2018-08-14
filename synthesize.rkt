#lang rosette

(require "sketch.rkt")
(require "ordering.rkt")
(require "parsers/parser.rkt")
(require "parsers/mask-constants.rkt")
(require "parsers/halide-dsl.rkt")
(require "parsers/insn-node-count.rkt")
(require "parsers/ordering.rkt")

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
                                               #:guarantee (assert (and (sketch-ordering-greater-than LHS RHS-sketch)
                                                                        (equal? evaled-LHS evaled-RHS))))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "no solution found")
                 (print-live-regs-sketch (evaluate RHS-sketch binding)))))))

(define (synth-rewrite-to-ordering RHS-sketch LHS min-sketch . inputs)
  (begin (clear-asserts!)
         (let ([evaled-LHS (apply (get-sketch-function LHS) inputs)]
               [evaled-RHS (apply (get-sketch-function RHS-sketch) inputs)])
           (begin
             (define binding (time (synthesize #:forall (symbolics inputs)
                                               #:guarantee (assert (and (sketch-ordering-greater-than LHS RHS-sketch)
                                                                        (sketch-ordering-greater-than RHS-sketch min-sketch)
                                                                        (equal? evaled-LHS evaled-RHS))))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "no solution found")
                 (print-live-regs-sketch (evaluate RHS-sketch binding)))))))

(define (synth-rewrite-from-testcase testcase)
  (let* ([var-count (get-variable-count testcase)]
       ;  [const-count (get-constant-count testcase)]
         [insn-count (get-insn-node-count testcase)]
         [sym-var-list (for/list ([i (range var-count)]) (get-sym-hld-int))]
       ;  [sym-const-list (for/list ([i (range const-count)]) (get-sym-hld-int))]
         [RHS-sketch (get-symbolic-sketch insn-count var-count 0)])
    (begin
      (clear-asserts!)
      (let ([evaled-LHS (evaluate-parser (parser-to-hld-dsl #f (build-var-lookup "v" sym-var-list) (make-hash '())) testcase)]
            [evaled-RHS (apply (get-sketch-function RHS-sketch) sym-var-list)])
        (begin
          (define binding (time (synthesize #:forall (symbolics sym-var-list)
                                            #:guarantee (assert (and (ordering-greater-than-sketch (find-ordering testcase) RHS-sketch)
                                                                     (equal? evaled-LHS evaled-RHS))))))
          (clear-asserts!)
          (if (unsat? binding)
              (displayln "no solution found")
              (print-live-regs-sketch (evaluate RHS-sketch binding))))))))

(define (verify-testcase s)
  (let* ([sym-vars (for/list ([i (range (get-variable-count s))]) (get-sym-hld-int))]
         [var-hash (build-var-lookup "v" sym-vars)]
         [p (parser-to-hld-dsl #f var-hash (make-hash '()))]
         [model (time (verify (assert (evaluate-parser p s))))])
    (unsat? model)))

(define (verify-wild-constants-testcase s)
  (let* ([sym-vars (for/list ([i (range (get-variable-count s))]) (get-sym-hld-int))]
         [sym-consts (for/list ([i (range (get-constant-count s))]) (get-sym-hld-int))]
         [var-hash (build-var-lookup "v" sym-vars)]
         [const-hash (build-var-lookup "c" sym-consts)]
         [p (parser-to-hld-dsl #t var-hash const-hash)])
    (unsat? model)))
