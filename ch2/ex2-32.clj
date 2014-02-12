(defn subsets [s]
  (if (empty? s)
    '(())
    (let [leftover (subsets (rest s))]
      (append leftover (map (fn [x] (cons (first s) x)) leftover)))))

(subsets '(1 2 3))
