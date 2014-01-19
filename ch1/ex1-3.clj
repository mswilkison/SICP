(defn square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn larger-sum-of-squares [x y z]
  (cond (and (< x y) (< x z)) (sum-of-squares y z)
        (and (< y x) (< y z)) (sum-of-squares x z)
        :else (sum-of-squares x y)))

