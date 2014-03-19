(defn =zero? [x] (apply-generic '=zero? x))

(defn zero-poly [x]
  (cond (number? x) (= x 0)
        (seq? x) false
        :else (throw (Exception. "Error -- Unknown type"))))

(put '=zero? 'polynomial zero-poly)
