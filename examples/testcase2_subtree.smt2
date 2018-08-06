(declare-const x Int)
(declare-const y Int)

(define-fun max ((a Int) (b Int)) Int
  (ite (<= a b) b a))

(define-fun min ((a Int) (b Int)) Int
  (ite (<= a b) a b))

(assert (< (min (+ x 12) y) (+ (min (+ x 20) y) (- 144))))

(check-sat)
(get-model)
