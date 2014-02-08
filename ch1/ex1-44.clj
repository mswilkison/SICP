(defn repeated [f n]
  (fn [x]
    (defn iter [fn-result n]
      (if (= n 1)
        fn-result
        (iter (f fn-result) (dec n))))
    (iter (f x) n)))

(defn smooth [f dx]
  (fn [x]
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(defn n-fold-smooth [f dx n]
  (repeated (smooth f dx) n))
