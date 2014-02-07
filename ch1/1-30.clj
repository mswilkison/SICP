(defn sum [term a step b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (step a) (+ result (term a)))))
  (iter a 0))
