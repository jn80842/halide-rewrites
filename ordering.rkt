#lang rosette

(require "lang.rkt")
(require "sketch.rkt")

(provide (all-defined-out))

(struct ordering ([nc-count #:mutable] histo-count) #:transparent)

(define (get-new-ordering)
  (ordering 0 (get-operator-histo)))

;; add one extra bucket for constant inputs
(define (get-operator-histo)
  (make-hash (map (λ (idx val) (cons idx val))
                  (range (add1 (length operator-list))) (make-list (add1 (length operator-list)) 0))))

(define (increment-ordering-nc-count o)
  (set-ordering-nc-count! o (add1 (ordering-nc-count o))))

(define (increment-ordering-operator o idx)
  (let ([histo (ordering-histo-count o)])
    (hash-set! histo idx (add1 (hash-ref histo idx)))))

(define (histo-hash-greater-than h1 h2)
  (let ([first-mismatch (findf (λ (p) (not (= (car p) (cdr p))))
                               (for/list ([i (reverse (range (add1 (length operator-list))))])
                                 (cons (hash-ref h1 i) (hash-ref h2 i))))])
    (if first-mismatch (> (car first-mismatch) (cdr first-mismatch)) #f)))

#;(define (histo-greater-than sk1 sk2)
  (let ([sk1-histo (build-operator-histo sk1)]
        [sk2-histo (build-operator-histo sk2)])
    (histo-hash-greater-than sk1-histo sk2-histo)))

(define (get-insn-nc-counts prior-insn-counts current-insn)
  (let ([arity (get-operator-arity-by-idx (insn-op-idx current-insn))])
    (+ (if (>= arity 3) (list-ref prior-insn-counts (insn-arg3-idx current-insn)) 0)
       (if (>= arity 2) (list-ref prior-insn-counts (insn-arg2-idx current-insn)) 0)
       (list-ref prior-insn-counts (insn-arg1-idx current-insn)))))

(define (build-nonconst-counts sk)
  (let ([nc-and-const-insn-list (append (make-list (sketch-nc-input-count sk) 1)
                                        (make-list (sketch-const-input-count sk) 0))])
    (letrec ([f (λ (insn-list count-list)
                  (cond [(empty? insn-list) count-list]
                        [else (f (cdr insn-list)
                                 (append count-list (list (get-insn-nc-counts count-list (car insn-list)))))]))])
    (f (sketch-insn-list sk) nc-and-const-insn-list))))

(define (build-operator-histo sk)
  (let ([h (get-operator-histo)])
    (begin (for ([i (range (length (sketch-insn-list sk)))])
             (let ([op (insn-op-idx (list-ref (sketch-insn-list sk) i))])
               (hash-set! h op (add1 (hash-ref h op)))))
           (hash-set! h (length operator-list) (sketch-const-input-count sk))
           h)))

(define (expr-nonconst-count sk)
  (let ([count-list (build-nonconst-counts sk)])
    (list-ref count-list (sketch-retval-idx sk))))

(define (get-sketch-ordering sk)
  (ordering (expr-nonconst-count sk) (build-operator-histo sk)))

(define (ordering-greater-than o1 o2)
  (or (> (ordering-nc-count o1) (ordering-nc-count o2))
      (histo-hash-greater-than (ordering-histo-count o1) (ordering-histo-count o2))))

#;(define (expr-greater-than sk1 sk2)
  (let ([order1 (get-sketch-ordering sk1)]
        [order2 (get-sketch-ordering sk2)])
    (or (> (expr-nonconst-count sk1) (expr-nonconst-count sk2))
        (histo-hash-greater-than (build-operator-histo sk1) (build-operator-histo sk2)))))

(define (ordering-greater-than-sketch order sk1)
  (or (> (ordering-nc-count order) (expr-nonconst-count sk1))
      (histo-hash-greater-than (ordering-histo-count order) (build-operator-histo sk1))))

(define (expr-greater-than sk1 sk2)
  (or (> (expr-nonconst-count sk1) (expr-nonconst-count sk2))
      (histo-hash-greater-than (build-operator-histo sk1) (build-operator-histo sk2))
     ; (histo-greater-than sk1 sk2)
      ;; lpo
      ))