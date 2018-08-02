#lang rosette

(require "lang.rkt")
(require "sketch.rkt")

;; add one extra bucket for constant inputs
(define (get-operator-histo)
  (make-hash (map (λ (idx val) (cons idx val)) (range (add1 (length operator-list))) (make-list (add1 (length operator-list)) 0))))

(define (build-operator-histo sk)
  (let ([h (get-operator-histo)])
    (begin (for ([i (range (length (sketch-insn-list sk)))])
             (let ([op (insn-op-idx (list-ref (sketch-insn-list sk) i))])
               (hash-set! h op (add1 (hash-ref h op)))))
           (hash-set! h (length operator-list) (sketch-const-input-count sk))
           h)))

(define (histo-greater-than sk1 sk2)
  (let* ([sk1-histo (build-operator-histo sk1)]
         [sk2-histo (build-operator-histo sk2)]
         [first-mismatch (findf (λ (p) (not (= (car p) (cdr p))))
                                (for/list ([i (range (add1 (length operator-list)))])
                                  (cons (hash-ref sk1-histo i) (hash-ref sk2-histo i))))])
    (> (car first-mismatch) (cdr first-mismatch))))

(define (get-insn-nc-counts prior-insn-counts current-insn)
  (let ([arity (procedure-arity (list-ref operator-list (insn-op-idx current-insn)))])
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

(define (expr-nonconst-count sk)
  (let ([count-list (build-nonconst-counts sk)])
    (list-ref count-list (sketch-retval-idx sk))))

(define (expr-greater-than sk1 sk2)
  (or (> (expr-nonconst-count sk1) (expr-nonconst-count sk2))
      (histo-greater-than sk1 sk2)
      ;; lpo
      ))