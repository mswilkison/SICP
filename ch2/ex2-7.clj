(defn make-interval [a b] (cons a [b]))

(defn upper-bound [interval]
  (last interval))

(defn lower-bound [interval]
  (first interval))
