(declare-const x Int)
(declare-const c0 Int)
(declare-const c1 Int)
(declare-const c2 Int)

(assert (< c0 c1))
(assert (< 0 c2))
(assert (not (<= (div (- c0 x) c2) (div (- c1 x) c2))))

(check-sat)
(get-model)
