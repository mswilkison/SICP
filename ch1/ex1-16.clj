; More efficient iterative implementation of expt function
; Leverages invariant quantity of a*b^n

(defn square [x]
  (* x x))

(defn fast-expt [b n]
  (defn fast-iter [counter a]
    (if (or (= counter 0) (= counter 1))
      a
      (fast-iter (/ counter 2) (* a (square b)))))
  (if (even? n)
    (fast-iter n 1)
    (fast-iter (- n 1) b)))