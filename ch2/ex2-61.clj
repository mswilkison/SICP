(defn adjoin-set [x set1]
  (cond (empty? set1) [x]
        (< x (first set1)) (cons x set1)
        (> x (first set1)) (cons (first set1)
                                 (adjoin-set x (rest set1)))
        :else set1))
