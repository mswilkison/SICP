(defn double-apply [f]
  (fn [x]
    (f (f x))))

((double-apply inc) 5)
(((double-apply (double-apply double-apply)) inc) 5)
