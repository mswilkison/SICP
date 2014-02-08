; Recursive implementation
(defn cont-frac [n d k]
  (if (= k 0)
    (n k)
    (/ (n k) (+ (d k) (cont-frac n d (dec k))))))

(cont-frac (fn [i] 1.0)
           (fn [i] 1.0)
           10)

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

(/ 1 (/ (+ 1 (sqrt 5)) 2)) ; 1 / golden ratio
