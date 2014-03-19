(defn first-term [L]
  (list (first L) (dec (length L))))

(defn adjoin-term [t L]
  (cond (=zero? (coeff t)) L
        (= (order t) (length L)
           (cons (coeff t) L))
        :else (adjoin-term t (cons 0 L))))
