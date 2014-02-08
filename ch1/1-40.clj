(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [step (f guess)]
      (if (close-enough? guess step)
        step
        (try-it step))))
  (try-it first-guess))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic [a b c]
  (fn [x] (+ (* x x x)
             (* a x x)
             (* b x)
             c)))
