(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [step (f guess)]
      (if (close-enough? guess step)
        step
        (try-it step))))
  (try-it first-guess))

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn repeated [f n]
  (fn [x]
    (defn iter [fn-result n]
      (if (= n 1)
        fn-result
        (iter (f fn-result) (dec n))))
    (iter (f x) n)))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn n-root [root x]
  (fixed-point-of-transform (fn [y] (/ x (Math/pow y (- root 1))))
                            (repeated average-damp (quot root 2))
                            1.0))
