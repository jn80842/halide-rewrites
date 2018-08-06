(declare-const x Int)

(assert (not (or (< (- 1) x) (< x (- 10)))))

;; (assert (not (or (< -1 x) (< x -10))))

(check-sat)
(get-model)
