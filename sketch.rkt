#lang rosette

(require "lang.rkt")

(provide (all-defined-out))

(define (get-sym-hld-int)
  ;(define-symbolic* indet-flag boolean?)
  ;(define-symbolic* const-flag boolean?)
  (define-symbolic* int integer?)
  (hld-int #f #f int))

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)

(define (harvest input-list)
  (for/list ([i (range (length input-list))])
    (if (hld-int? (list-ref input-list i))
        (hld-int-val (list-ref input-list i))
        (list-ref input-list i))))

;; a halide expression sketch is a list of instructions and a return value
;; each instruction has an operator index and indices of the operator's arguments
;; the return value is the index of the register whose value should be returned

(struct insn (op-idx arg1-idx arg2-idx arg3-idx) #:transparent)

(define (call-insn i registers)
  (let ([arity (get-arity-by-idx (insn-op-idx i))]
        [op-func (list-ref operator-list (insn-op-idx i))])
    (case arity
      [(1) (op-func (list-ref registers (insn-arg1-idx i)))]
      [(2) (op-func (list-ref registers (insn-arg1-idx i)) (list-ref registers (insn-arg2-idx i)))]
      [(3) (op-func (list-ref registers (insn-arg1-idx i)) (list-ref registers (insn-arg2-idx i)) (list-ref registers (insn-arg3-idx i)))])))

(define (get-sym-insn)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (insn op arg1 arg2 arg3))

(struct sketch (insn-list retval-idx nc-input-count const-input-count) #:transparent)

(define (get-symbolic-sketch insn-count nc-input-count const-input-count)
  (define-symbolic* retval integer?)
  (sketch (for/list ([i (range insn-count)]) (get-sym-insn)) retval nc-input-count const-input-count))

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-regs i)
                (cond [(equal? (length (sketch-insn-list sk)) i) calculated-regs]
                      [else (let ([next-reg (call-insn (list-ref (sketch-insn-list sk) i) calculated-regs)])
                              (f (append calculated-regs (list next-reg)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))






