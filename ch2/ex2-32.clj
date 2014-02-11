(defn subsets [s]
  (if (empty? s)
    '(())
    (let [leftover (subsets (rest s))]
      (append leftover (map (fn [x] (cons (first s) x)) leftover)))))

(subsets '(1 2 3))

(append '(()) '(4 5))

(empty? (rest '(3)))
(rest '(3))
'()
