#lang racket

(require rackunit)

(require "../lang.rkt")
(require "../sketch.rkt")
(require "../ordering.rkt")

(define add-sk (sketch (list (insn add-idx 0 1 0))
                       2 2 0))
(define sub-sk (sketch (list (insn sub-idx 0 1 0))
                       2 2 0))

(define x+c0/c1 (sketch (list (insn 0 0 1 0)
                              (insn 4 3 2 0))
                        4
                        1
                        2))

(define x+c0*c1 (sketch (list (insn 0 0 1 0)
                              (insn 3 3 2 0))
                        4 1 2))

(define x+c0*x+c0 (sketch (list (insn 0 0 1 0)
                                (insn 3 2 2 0))
                          3
                          1
                          1))

(define dead-reg-sk (sketch (list (insn 0 0 1 0)
                                  (insn 1 0 1 0))
                            1 1 0))

(check-equal? (get-sketch-histo-count x+c0/c1)
              '(1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0))
(check-equal? (get-sketch-histo-count x+c0*c1)
              '(1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal? (get-sketch-histo-count x+c0*x+c0)
              '(2 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal? (get-sketch-histo-count dead-reg-sk)
              '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(check-false (sketch-histo-greater-than add-sk sub-sk))
(check-true (sketch-histo-greater-than x+c0/c1 x+c0*c1))

(check-eq? (expr-nonconst-count x+c0/c1) 1)
(check-eq? (expr-nonconst-count x+c0*x+c0) 2)

(check-true (sketch-ordering-greater-than x+c0*x+c0 x+c0/c1))
(check-true (sketch-ordering-greater-than x+c0/c1 x+c0*c1))
(check-true (sketch-ordering-greater-than x+c0*x+c0 x+c0*c1))
