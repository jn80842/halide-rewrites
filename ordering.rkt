#lang rosette

(require "lang.rkt")
(require "sketch.rkt")

(provide (all-defined-out))

(struct ordering ([nc-count #:mutable] histo-count) #:transparent)

(define (get-sketch-histo-count sk)
  (let ([op-list (map (λ (i) (insn-op-idx i)) (take (sketch-insn-list sk) (- (add1 (sketch-retval-idx sk))
                                                                               (+ (sketch-nc-input-count sk) (sketch-const-input-count sk)))))])
    (map (λ (n) (foldl (λ (op i) (if (equal? op n) (add1 i) i)) 0 op-list))
       (range (length operator-list)))))

(define (sketch-histo-greater-than sk1 sk2)
  (histo-greater-than (get-sketch-histo-count sk1) (get-sketch-histo-count sk2)))

(define (histo-greater-than h1 h2)
  (findf (λ (x) (not (equal? x 'tie)))
         (reverse (map (λ (count1 count2)
                         (if (equal? count1 count2)
                             'tie
                             (> count1 count2))) h1 h2))))

(define (get-insn-nc-counts prior-insn-counts current-insn)
  (+ (list-ref prior-insn-counts (insn-arg1-idx current-insn))
     (list-ref prior-insn-counts (insn-arg2-idx current-insn))
     (list-ref prior-insn-counts (insn-arg3-idx current-insn))))

(define (build-nonconst-counts sk)
  (let ([nc-and-const-insn-list (append (make-list (sketch-nc-input-count sk) 1)
                                        (make-list (sketch-const-input-count sk) 0))])
    (letrec ([f (λ (insn-list count-list)
                  (cond [(empty? insn-list) count-list]
                        [else (f (cdr insn-list)
                                 (append count-list (list (get-insn-nc-counts count-list (car insn-list)))))]))])
    (f (sketch-insn-list sk) nc-and-const-insn-list))))

(define (expr-nonconst-count sk)
  (let ([count-list (build-nonconst-counts sk)])
    (list-ref count-list (sketch-retval-idx sk))))

(define (get-sketch-ordering sk)
  (ordering (expr-nonconst-count sk) (get-sketch-histo-count sk)))

(define (ordering-greater-than o1 o2)
  (or (> (ordering-nc-count o1) (ordering-nc-count o2))
      (histo-greater-than (ordering-histo-count o1) (ordering-histo-count o2))))

(define (ordering-greater-than-sketch order sk1)
  (or (> (ordering-nc-count order) (expr-nonconst-count sk1))
      (histo-greater-than (ordering-histo-count order) (get-sketch-histo-count sk1))))

(define (sketch-ordering-greater-than sk1 sk2)
  (or (> (expr-nonconst-count sk1) (expr-nonconst-count sk2))
      (sketch-histo-greater-than sk1 sk2)
     ; (histo-hash-greater-than (build-operator-histo sk1) (build-operator-histo sk2))
     ; (histo-greater-than sk1 sk2)
      ;; lpo
      ))

(define minimum-ordered-sketch (sketch '() 0 1 0))