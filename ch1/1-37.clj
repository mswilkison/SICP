; Recursive implementation
(defn cont-frac [n d k]
  (defn frac [i]
    (if (< i k)
      (/ (n i) (+ (d i) (frac (inc i))))
      (/ (n i) (d i))))
  (frac 1))

(cont-frac (fn [i] 1.0)
           (fn [i] 1.0)
           11)

; Iterative implementation
(defn cont-frac-iter [n d k]
  (defn iter [i result]
    (if (= i 0)
      result
      (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter (dec k) (/ (n k) (d k))))

(cont-frac-iter (fn [i] 1.0)
                (fn [i] 1.0)
                11)

(/ 1 (/ (+ 1 (Math/sqrt 5)) 2)) ; 1 / golden ratio
