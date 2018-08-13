#lang racket

(require rackunit)

(require "../lang.rkt")
(require "../sketch.rkt")
(require "../ordering.rkt")

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

(check-eq? (expr-nonconst-count x+c0/c1) 1)
(check-eq? (expr-nonconst-count x+c0*x+c0) 2)

(check-true (histo-greater-than x+c0/c1 x+c0*c1))

(check-true (expr-greater-than x+c0*x+c0 x+c0/c1))
(check-true (expr-greater-than x+c0/c1 x+c0*c1))
(check-true (expr-greater-than x+c0*x+c0 x+c0*c1))

(check-true (ordering-greater-than (get-sketch-ordering x+c0*x+c0) (get-sketch-ordering x+c0/c1)))
(check-true (ordering-greater-than (get-sketch-ordering x+c0/c1) (get-sketch-ordering x+c0*c1)))
(check-true (ordering-greater-than (get-sketch-ordering x+c0*x+c0) (get-sketch-ordering x+c0*c1)))