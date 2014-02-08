(defn square [x]
  (* x x))

(defn cont-frac [n d k]
  (defn frac [i]
    (if (< i k)
      (/ (n i) (+ (d i) (frac (inc i))))
      (/ (n i) (d i))))
  (frac 1))

(defn tan-cf [x k]
  (defn n [i]
    (if (= i 1)
      x
      (- (square x))))
  (defn d [i]
    (- (* 2 i) 1))
  (cont-frac n d k))
