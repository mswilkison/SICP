(defn cont-frac [n d k]
  (defn frac [i]
    (if (< i k)
      (/ (n i) (+ (d i) (frac (inc i))))
      (/ (n i) (d i))))
  (frac 1))

(defn d [i]
  (if-not (= 0 (mod (+ i 1) 3))
    1.0
    (* 2 (/ (+ i 1) 3))))

(def e (+ 2 (cont-frac (fn [i] 1.0)
                       d
                       10)))

e
