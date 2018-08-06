(declare-const x Int)

(assert (= -1 (div (- 0 (mod (+ x 7) 136)) 136)))

(check-sat)
(get-model)
