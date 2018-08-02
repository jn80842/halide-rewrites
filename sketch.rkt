#lang rosette

;(require "lang.rkt")

(provide (all-defined-out))

;; a halide expression sketch is a list of instructions and a return value
;; each instruction has an operator index and indices of the operator's arguments
;; the return value is the index of the register whose value should be returned

(struct insn (op-idx arg1-idx arg2-idx arg3-idx) #:transparent)

(define (get-sym-insn)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (insn op arg1 arg2 arg3))

(struct sketch (insn-list retval-idx nc-input-count const-input-count) #:transparent)




;; (x + c0) / c1
(define s (sketch (list (insn 0 0 1 0)
                        (insn 4 3 2 0)) 2 1 2))


  
