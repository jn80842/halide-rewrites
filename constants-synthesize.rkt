#lang rosette

(require "constants-lang.rkt")

(current-bitwidth #f)

(define (get-sym-int)
  (begin
    (define-symbolic* x integer?)
    x))

(struct insn (op-idx arg1-idx arg2-idx) #:transparent)

(define (call-insn i registers)
  ((get-operator-function-by-idx (insn-op-idx i)) (list-ref registers (insn-arg1-idx i)) (list-ref registers (insn-arg2-idx i))))

(define (get-sym-insn)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (insn op arg1 arg2))

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

(define (synthesize-predicate expr sk nc-inputs const-inputs)
  (let* ([sym-consts (map (λ (i) (get-sym-int)) const-inputs)]
         [evaled-sketch (apply (get-sketch-function sk) sym-consts)]
         [evaled-const-sketch (apply (get-sketch-function sk) const-inputs)]
         [evaled-expr (apply expr (append nc-inputs sym-consts))]
         [binding (time (synthesize #:forall (append nc-inputs sym-consts)
                                    #:guarantee (assert (and (or (not evaled-sketch) evaled-expr)
                                                             evaled-const-sketch))))])
    (if (unsat? binding)
        (displayln "no solution")
        (displayln binding))))

;; ((((min(v3, 0) + ((v1*63) + v2))*2) + -1) <= (((v1*63) + v2)*2))
;;(((a - x)/b) >= ((((a - x)/b) + (((c - x)/b) - ((a - x)/b))) - d))

(define (testcase-expr x y z)
  (<= (+ (+ (min z 0) (* (+ (* x 63) y) 2)) -1)
      (* (+ (* x 63) y) 2)))

(define (testcase-c-expr x y z a b c d)
  (<= (+ (* (+ (min z a) (+ (* x b) y)) c) (- d))
      (* (+ (* x b) y) c)))

(define-symbolic* x integer?)
(define-symbolic* y integer?)
(define-symbolic* z integer?)
(define-symbolic* a integer?)
(define-symbolic* b integer?)
(define-symbolic* c integer?)
(define-symbolic* d integer?)

(define predicate-sk (get-symbolic-sketch 4 4 0))

(define minified-expr
  (sketch (list (insn min-idx 2 7)
                (insn mul-idx 5 8)
                (insn le-idx 9 6))
          10 3 4))

(define predicate-answer
  (sketch (list (insn gt-idx 5 7)
                (insn mul-idx 3 5)
                (insn ge-idx 6 9)
                (insn and-idx 8 10))
          11 3 4))