(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(defn map-accum [p lst]
  (accumulate (fn [x y] (cons (p x) y)) nil lst))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn length [lst]
  (accumulate (fn [x y] (inc y)) 0 lst))
