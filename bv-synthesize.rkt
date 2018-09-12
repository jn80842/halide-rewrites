#lang rosette

(provide (all-defined-out))

(define bvw 7)

(current-bitwidth bvw)

(define (get-sym-bv)
  (define-symbolic* b (bitvector bvw))
  b)

;; Expr -> IntExpr | BoolExpr;
;; 
;; IntExpr -> IntExpr + IntExpr
;;         | IntExpr - IntExpr
;;         | IntExpr * IntExpr
;;         | IntExpr / IntExpr
;;         | IntExpr % IntExpr
;;         | max(IntExpr, IntExpr)
;;         | min(IntExpr, IntExpr)
;;         | Int;
;; 
;; BoolExpr -> ! BoolExpr
;;         | BoolExpr && BoolExpr
;;         | BoolExpr || BoolExpr
;;         | IntExpr < IntExpr
;;         | IntExpr - Intexpr == 0
;;         | BoolExpr == BoolExpr
;;         | Bool;

(define (bvsmax b1 b2)
  (if (bvsgt b1 b2) b1 b2))

(define (bvsmin b1 b2)
  (if (bvsgt b1 b2) b2 b1))

(define (bvnot-hld b1 b2)
  (bvnot b1))

(define (noop b1 b2)
  b1)

(define operators-list
  (list bvadd
        bvsub
        bvmul
        bvsdiv
        bvsmod
        bvsmax
        bvsmin
        bvand
        bvor
        bvslt
        bvsle
        bvsgt
        bvsge
        bveq
        noop))

(define (get-operator-function-by-idx idx)
  (if (bveq idx (bv 0 bvw))
      bvadd
      (if (bveq idx (bv 1 bvw))
          bvsub
          (if (bveq idx (bv 2 bvw))
              bvmul
              (if (bveq idx (bv 3 bvw))
                  bvsdiv
                  (if (bveq idx (bv 4 bvw))
                      bvsmod
                      (if (bveq idx (bv 5 bvw))
                          bvsmax
                          (if (bveq idx (bv 6 bvw))
                              bvsmin
                              (if (bveq idx (bv 7 bvw))
                                  bvand
                                  (if (bveq idx (bv 8 bvw))
                                      bvor
                                      (if (bveq idx (bv 9 bvw))
                                          bvslt
                                          (if (bveq idx (bv 10 bvw))
                                              bvsle
                                              (if (bveq idx (bv 11 bvw))
                                                  bvsgt
                                                  (if (bveq idx (bv 12 bvw))
                                                      bvsge
                                                      (if (bveq idx (bv 13 bvw))
                                                          bveq
                                                          (if (bveq idx (bv 14 bvw))
                                                              bvnot-hld
                                                              noop))))))))))))))))
(define (get-operator-name-by-idx idx)
  (if (bveq idx (bv 0 bvw))
      "bvadd"
      (if (bveq idx (bv 1 bvw))
          "bvsub"
          (if (bveq idx (bv 2 bvw))
              "bvmul"
              (if (bveq idx (bv 3 bvw))
                  "bvsdiv"
                  (if (bveq idx (bv 4 bvw))
                      "bvsmod"
                      (if (bveq idx (bv 5 bvw))
                          "bvsmax"
                          (if (bveq idx (bv 6 bvw))
                              "bvsmin"
                              (if (bveq idx (bv 7 bvw))
                                  "bvand"
                                  (if (bveq idx (bv 8 bvw))
                                      "bvor"
                                      (if (bveq idx (bv 9 bvw))
                                          "bvslt"
                                          (if (bveq idx (bv 10 bvw))
                                              "bvsle"
                                              (if (bveq idx (bv 11 bvw))
                                                  "bvsgt"
                                                  (if (bveq idx (bv 12 bvw))
                                                      "bvsge"
                                                      (if (bveq idx (bv 13 bvw))
                                                          "bveq"
                                                          (if (bveq idx (bv 14 bvw))
                                                              "bvnot-hld"
                                                              "noop"))))))))))))))))
(define get-op get-operator-function-by-idx)

(define (get-fixed-input idx i0 i1 i2 i3)
  (if (bveq idx (bv 0 bvw))
      i0
      (if (bveq idx (bv 1 bvw))
          i1
          (if (bveq idx (bv 2 bvw))
              i2 i3))))

(define (get-fixed-input3 idx inputs)
  (if (bveq idx (bv 0 bvw))
      (list-ref inputs 0)
      (if (bveq idx (bv 1 bvw))
          (list-ref inputs 1)
          (if (bveq idx (bv 2 bvw))
              (list-ref inputs 2)
              (if (bveq idx (bv 3 bvw))
                  (list-ref inputs 3)
                  (if (bveq idx (bv 4 bvw))
                      (list-ref inputs 4)
                      (if (bveq idx (bv 5 bvw))
                          (list-ref inputs 5)
                          (if (bveq idx (bv 6 bvw))
                              (list-ref inputs 6)
                              (list-ref inputs 7)))))))))

(define (get-fixed-input-names3 idx)
  (let ([inputs (for/list ([i (range (expt 2 3))]) (string-append "b" (number->string i)))])
    (if (bveq idx (bv 0 bvw))
      (list-ref inputs 0)
      (if (bveq idx (bv 1 bvw))
          (list-ref inputs 1)
          (if (bveq idx (bv 2 bvw))
              (list-ref inputs 2)
              (if (bveq idx (bv 3 bvw))
                  (list-ref inputs 3)
                  (if (bveq idx (bv 4 bvw))
                      (list-ref inputs 4)
                      (if (bveq idx (bv 5 bvw))
                          (list-ref inputs 5)
                          (if (bveq idx (bv 6 bvw))
                              (list-ref inputs 6)
                              (list-ref inputs 7))))))))))

(struct fixedsk2 (op0 op1 op2 input0 input1 input2 input3) #:transparent)

(struct fixedsk3 (op0 op1 op2 op3 op4 op5 op6 idx0 idx1 idx2 idx3 idx4 idx5 idx6 idx7) #:transparent)

(define (get-fixed-symbolic-sketch2)
  (let ([ops (for/list ([i (range (sub1 (expt 2 2)))]) (get-sym-bv))]
        [indexes (for/list ([i (range (expt 2 3))]) (get-sym-bv))])
    (apply fixedsk2 (append ops indexes))))

(define (get-fixed-symbolic-sketch3)
  (let ([ops (for/list ([i (range (sub1 (expt 2 3)))]) (get-sym-bv))]
        [indexes (for/list ([i (range (expt 2 3))]) (get-sym-bv))])
    (apply fixedsk3 (append ops indexes))))

(define (get-sketch-function2 fsk)
  (λ (i0 i1 i2 i3)
    ((get-operator-function-by-idx (fixedsk2-op2 fsk)) ((get-operator-function-by-idx (fixedsk2-op0 fsk)) (get-fixed-input (fixedsk2-input0 fsk) i0 i1 i2 i3)
                                                                                                          (get-fixed-input (fixedsk2-input1 fsk) i0 i1 i2 i3))
                                                       ((get-operator-function-by-idx (fixedsk2-op1 fsk)) (get-fixed-input (fixedsk2-input2 fsk) i0 i1 i2 i3)
                                                                                                          (get-fixed-input (fixedsk2-input3 fsk) i0 i1 i2 i3)))))
(define (get-sketch-function3 fsk)
  (λ (i0 i1 i2 i3 i4 i5 i6 i7)
    (let ([inputs (list i0 i1 i2 i3 i4 i5 i6 i7)])
      ((get-op (fixedsk3-op0 fsk)) ((get-op (fixedsk3-op1 fsk)) ((get-op (fixedsk3-op3 fsk)) (get-fixed-input3 (fixedsk3-idx0 fsk) inputs)
                                                                                             (get-fixed-input3 (fixedsk3-idx1 fsk) inputs))
                                                                ((get-op (fixedsk3-op4 fsk)) (get-fixed-input3 (fixedsk3-idx2 fsk) inputs)
                                                                                             (get-fixed-input3 (fixedsk3-idx3 fsk) inputs)))
                                   ((get-op (fixedsk3-op2 fsk)) ((get-op (fixedsk3-op5 fsk)) (get-fixed-input3 (fixedsk3-idx4 fsk) inputs)
                                                                                             (get-fixed-input3 (fixedsk3-idx5 fsk) inputs))
                                                                ((get-op (fixedsk3-op6 fsk)) (get-fixed-input3 (fixedsk3-idx6 fsk) inputs)
                                                                                             (get-fixed-input3 (fixedsk3-idx7 fsk) inputs)))))))
(define (get-string-sketch-function3 fsk)
    (string-append "(define (synthesized-function "
                   (string-join (for/list ([i (range (expt 2 3))]) (string-append "b" (number->string i))) " ")
                   ")\n"
                   "  (" (get-operator-name-by-idx (fixedsk3-op0 fsk)) " "
                   "(" (get-operator-name-by-idx (fixedsk3-op1 fsk)) " "
                   "(" (get-operator-name-by-idx (fixedsk3-op3 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx0 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx1 fsk)) ") "
                   "(" (get-operator-name-by-idx (fixedsk3-op4 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx2 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx3 fsk)) ")) "
                   "(" (get-operator-name-by-idx (fixedsk3-op2 fsk)) " "
                   "(" (get-operator-name-by-idx (fixedsk3-op5 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx4 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx5 fsk)) ") "
                   "(" (get-operator-name-by-idx (fixedsk3-op6 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx6 fsk)) " "
                   (get-fixed-input-names3 (fixedsk3-idx7 fsk)) "))))"))

#;(define (synthesize-predicate expr sk nc-inputs const-inputs)
  (let* ([sym-consts (append (map (λ (i) (get-sym-bv)) (list-tail const-inputs 1)) (list (bv 0 bvw)))]
         [evaled-sketch (apply (get-sketch-function sk) sym-consts)]
         [evaled-const-sketch (apply (get-sketch-function sk) const-inputs)]
         [evaled-expr (apply expr (append nc-inputs sym-consts))]
         [binding (time (synthesize #:forall (append nc-inputs sym-consts)
                                    #:guarantee (assert (and (or (not evaled-sketch) evaled-expr)
                                                             evaled-const-sketch))))])
    (if (unsat? binding)
        (displayln "no solution")
        (displayln binding))))

(define (testcase-c-expr x y z a b c d)
  (bvsle (bvadd (bvmul (bvadd (bvsmin z a) (bvadd (bvmul x b) y)) c) (bvsub (bv 0 7) d))
      (bvmul (bvadd (bvmul x b) y) c)))

(define-symbolic* bx (bitvector 7))
(define-symbolic* by (bitvector 7))
(define-symbolic* bz (bitvector 7))
(define-symbolic* bw (bitvector 7))