#lang rosette

(require rackunit)

(require "../lang.rkt")

(check-equal? (euclidean-div 6 3) 2)
(check-equal? (euclidean-div -6 3) -2)
(check-equal? (euclidean-div 6 -3) -2)
(check-equal? (euclidean-div -6 -3) 2)

(check-equal? (euclidean-div 3 2) 1)
(check-equal? (euclidean-div -3 2) -2)
(check-equal? (euclidean-div 3 -2) -1)
(check-equal? (euclidean-div -3 -2) 2)

(check-equal? (euclidean-mod 3 2) 1)
(check-equal? (euclidean-mod -3 2) 1)
(check-equal? (euclidean-mod 3 -2) 1)
(check-equal? (euclidean-mod -3 -2) 1)

(check-equal? (euclidean-mod 25 3) 1)
(check-equal? (euclidean-mod -25 3) 2)
(check-equal? (euclidean-mod 25 -3) 1)
(check-equal? (euclidean-mod -25 -3) 2)

(define three (hld-int #f #f 3))
(define two (hld-int #f #f 2))
(define one (hld-int #f #f 1))
(define zero (hld-int #f #f 0))
(define negative-one (hld-int #f #f -1))
(define negative-two (hld-int #f #f -2))


(check-equal? (hld-add one negative-one) zero)

(check-equal? (hld-sub one negative-one) two)

(check-equal? (hld-mod three two) one)

(check-equal? (hld-div three two) one)

(check-equal? (hld-min negative-two zero) negative-two)

(check-equal? (hld-max negative-two zero) zero)

(check-true (hld-eq-int negative-one negative-one))
(check-false (hld-eq-int negative-one negative-two))

(check-true (hld-eq-bool #t #t))
(check-true (hld-eq-bool #f #f))
(check-false (hld-eq-bool #t #f))
(check-false (hld-eq-bool #f #t))

(check-true (hld-lt negative-two negative-one))
(check-false (hld-lt negative-two negative-two))
(check-false (hld-lt zero negative-two))

(check-true (hld-and #t #t))
(check-false (hld-and #t #f))
(check-equal? (hld-and one #t) 'failed-typecheck)

(check-true (hld-or #t #f))
(check-false (hld-or #f #f))
(check-equal? (hld-or one #f) 'failed-typecheck)

(check-true (hld-not #f))
(check-false (hld-not #t))
(check-equal? (hld-not one) 'failed-typecheck)

(check-equal? (hld-select-int #t one two) one)
(check-equal? (hld-select-int #f one two) two)
(check-equal? (hld-select-int one one two) 'failed-typecheck)
(check-equal? (hld-select-int #t #f two) 'failed-typecheck)

(check-true (hld-select-bool #t #t #f))
(check-false (hld-select-bool #f #t #f))
(check-equal? (hld-select-bool one #t #f) 'failed-typecheck)
(check-equal? (hld-select-bool #t one two) 'failed-typecheck)