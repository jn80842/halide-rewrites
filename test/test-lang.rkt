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