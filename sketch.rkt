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

;; a halide expression sketch is a list of instructions and a return value
;; each instruction has an operator index and indices of the operator's arguments
;; the return value is the index of the register whose value should be returned

(struct insn (op-idx arg1-idx arg2-idx arg3-idx) #:transparent)

(define (call-insn i registers)
  (let ([arity (get-operator-arity-by-idx (insn-op-idx i))]
        [op-func (get-operator-function-by-idx (insn-op-idx i))])
    (case arity
      [(1) (op-func (list-ref registers (insn-arg1-idx i)))]
      [(2) (op-func (list-ref registers (insn-arg1-idx i)) (list-ref registers (insn-arg2-idx i)))]
      [(3) (op-func (list-ref registers (insn-arg1-idx i)) (list-ref registers (insn-arg2-idx i)) (list-ref registers (insn-arg3-idx i)))])))

(define (args->string-list sk)
  (append (for/list ([i (range (sketch-nc-input-count sk))]) (format "_~a" i))
          (for/list ([j (range (sketch-nc-input-count sk) (+ (sketch-nc-input-count sk) (sketch-const-input-count sk)))]) (format "c~a" j))))

(define (inputs->string-list sk)
  (let ([args (args->string-list sk)])
    (for/list ([i (range (length args))])
      (format "  (define R~a ~a)" i (list-ref args i)))))

(define (insn-args->string i)
  (case (get-operator-arity-by-idx (insn-op-idx i))
    [(1) (format "R~a" (number->string (insn-arg1-idx i)))]
    [(2) (format "R~a R~a" (number->string (insn-arg1-idx i)) (number->string (insn-arg2-idx i)))]
    [(3) (format "R~a R~a R~a" (number->string (insn-arg1-idx i)) (number->string (insn-arg2-idx i)) (number->string (insn-arg3-idx i)))]))

(define (insns->string-list sk)
  (let ([input-offset (+ (sketch-nc-input-count sk) (sketch-const-input-count sk))])
    (for/list ([i (range (length (sketch-insn-list sk)))])
      (let ([current-insn (list-ref (sketch-insn-list sk) i)])
        (format "  (define R~a (~a ~a))" (+ input-offset i) (get-operator-name-by-idx (insn-op-idx current-insn)) (insn-args->string current-insn))))))

(define (sketch->string sk)
  (append (list (format "(define (sketch-function ~a)" (string-join (args->string-list sk) " ")))
          (inputs->string-list sk)
          (insns->string-list sk)
          (list (format "  R~a)" (sketch-retval-idx sk)))))

(define (live-reg-sketch->string sk)
  (let ([live-regs (find-live-registers sk)]
        [input-count (+ (sketch-nc-input-count sk) (sketch-const-input-count sk))]
        [input-strings (inputs->string-list sk)]
        [insn-strings (insns->string-list sk)])
    (append (list (format "(define (sketch-function ~a)" (string-join (args->string-list sk) " ")))
            (filter identity (map (λ (x) (if (member x live-regs)
                                             (list-ref input-strings x)
                                             #f))
                                  (range input-count)))
            (filter identity (map (λ (x) (if (member x live-regs)
                                             (list-ref insn-strings (- x input-count))
                                             #f))
                                  (range input-count (+ input-count (length (sketch-insn-list sk))))))
            (list (format "  R~a)" (sketch-retval-idx sk))))))

(define (find-referenced-registers i)
  (let ([arity (get-operator-arity-by-idx (insn-op-idx i))])
    (case arity
      [(1) (list (insn-arg1-idx i))]
      [(2) (list (insn-arg1-idx i) (insn-arg2-idx i))]
      [(3) (list (insn-arg1-idx i) (insn-arg2-idx i) (insn-arg3-idx i))])))

(define (find-live-registers sk)
  (let ([input-count (+ (sketch-nc-input-count sk) (sketch-const-input-count sk))])
    (letrec ([f (λ (unprocessed-idx live-regs)
                  (cond [(empty? unprocessed-idx) live-regs]
                        [(< (first unprocessed-idx) input-count) (f (cdr unprocessed-idx) (append (list (first unprocessed-idx)) live-regs))]
                        [else (f (append (cdr unprocessed-idx) (find-referenced-registers (list-ref (sketch-insn-list sk) (- (first unprocessed-idx) input-count))))
                                 (cons (first unprocessed-idx) live-regs))]))])
      (f (list (sketch-retval-idx sk)) '()))))

(define (print-sketch sk)
  (displayln (string-join (sketch->string sk) "\n")))

(define (print-live-regs-sketch sk)
  (displayln (string-join (live-reg-sketch->string sk) "\n")))

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






