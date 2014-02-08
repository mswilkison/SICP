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

(fixed-point (fn [x] (+ 1 (/ 1 x))) 2) ; fixed-point of (1 + 1/x)
(/ (+ 1 (sqrt 5)) 2) ; golden ratio
