(defn for-each [f lst]
  (if-not (empty? lst)
    (do
      (f (first lst))
      (for-each f (rest lst)))))

(for-each (fn [x] (newline) (println x))
          '(57 321 88))
