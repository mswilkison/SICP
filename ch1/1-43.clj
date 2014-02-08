(defn repeated [f n]
  (fn [x]
    (defn iter [fn-result n]
      (if (= n 1)
        fn-result
        (iter (f fn-result) (dec n))))
    (iter (f x) n)))

((repeated square 2) 5)
