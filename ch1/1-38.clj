(defn cont-frac [n d k]
  (if (= k 0)
    (n k)
    (/ (n k) (+ (d k) (cont-frac n d (dec k))))))

(defn d [i]
  (if-not (= 0 (mod (+ i 1) 3))
    1.0
    (* 2 (/ (+ i 1) 3))))

(def e (+ 2 (cont-frac (fn [i] 1.0)
                       d
                       10)))

e
