(declare-const v0 Int)
(declare-const v1 Int)
(declare-const v2 Int)

(define-fun min ((a Int) (b Int)) Int
  (ite (<= a b) a b))

(assert (not (not (and (< 0 v0)
                       (< (min (+ v1 12) v2) (+ (min (+ v1 20) v2) -144))))))

(check-sat)
(get-model)
