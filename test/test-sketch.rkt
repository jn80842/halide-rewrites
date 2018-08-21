#lang rosette

(require rackunit)

(require "../lang.rkt")
(require "../sketch.rkt")

(define x (get-sym-hld-int))
(define y (get-sym-hld-int))
(define zero (hld-constant 0))
(define -one (hld-constant -1))
(define -ten (hld-constant -10))

(define sk1 (sketch (list (insn add-idx 0 1 2))
                    3 2 0))

(check-equal? (get-sketch-input-count sk1) 3)
(check-equal? (get-sketch-register-count sk1) 4)
(check-equal? (get-variable-count-for-program sk1) 2)
(check-true (unsat? (verify (assert (equal? ((get-sketch-function sk1) x y) (hld-add x y #f))))))

(define sk2 (sketch (list (insn lt-idx 1 0 1)
                          (insn lt-idx 0 2 1)
                          (insn or-idx 4 5 1))
                    6
                    1
                    2))

(check-equal? (get-sketch-input-count sk2) 4)
(check-equal? (get-sketch-register-count sk2) 7)
(check-equal? (get-variable-count-for-program sk2) 2)
(check-true (unsat? (verify (assert (equal? ((get-sketch-function sk2) x -one -ten) (hld-or (hld-lt -one x #f) (hld-lt x -ten #f) #f))))))

(define sk3 (sketch (list (insn sub-idx 0 1 2))
                    6 2 3))

(check-true (unsat? (verify (assert (equal? ((get-sketch-function sk3) x y zero -one -ten) (hld-sub x y #f))))))