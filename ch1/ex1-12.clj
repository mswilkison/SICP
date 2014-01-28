(defn triangle [row col]
  (cond (< row col) false
        (or (= col 0) (= row col)) 1
        :else
        (+ (triangle (- row 1) (- col 1))
           (triangle (- row 1) col))))
