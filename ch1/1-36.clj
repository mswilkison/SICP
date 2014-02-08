(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-it [guess counter]
    (let [step (f guess)]
      (println "****")
      (println (str "Step: " counter))
      (println step)
      (if (close-enough? guess step)
        step
        (try-it step (inc counter)))))
  (try-it first-guess 1))

(fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 2)
; 34 steps without average damping

(defn average [x y]
  (/ (+ x y) 2))

(fixed-point (fn [x] (average x (/ (Math/log 1000) (Math/log x)))) 2)
; 9 steps with average damping
