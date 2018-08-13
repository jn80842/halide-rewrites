#lang racket

(require rackunit)
(require "../lang.rkt")
(require "../ordering.rkt")
(require "../parsers/parser.rkt")
(require "../parsers/halide-dsl.rkt")
(require "../parsers/ordering.rkt")

(define p1 "(v0 + (v1 - v0))")
(define h3 (hld-int #f #f 3))
(define h10 (hld-int #f #f 10))
(define order1 (ordering 2 (make-hash (list '(0 . 1)
                                            '(1 . 1)
                                            '(2 . 0)
                                            '(3 . 0)
                                            '(4 . 0)
                                            '(5 . 0)
                                            '(6 . 0)
                                            '(7 . 0)
                                            '(8 . 0)
                                            '(9 . 0)
                                            '(10 . 0)
                                            '(11 . 0)
                                            '(12 . 0)
                                            '(13 . 0)
                                            '(14 . 0)
                                            '(15 . 0)
                                            '(16 . 0)))))

(check-equal? (evaluate-parser (parser-to-hld-dsl #f (build-var-lookup "v" (list h3 h10)) (make-hash '())) p1) h10)

(check-equal? (find-ordering p1) order1)


