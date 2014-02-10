(def zero (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(add-1 zero)

(def one (fn [f] (fn [x] (f x))))

(def two (fn [f] (fn [x] (f (f x)))))

(defn add [m n]
  (fn [f] (fn [x] ((m f) ((n f) x)))))
