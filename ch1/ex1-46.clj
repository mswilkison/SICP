(defn iterative-improve [good-enough? improve-guess]
  (fn [guess]
    (defn iter [guess]
      (if (good-enough? guess)
        guess
        (iter (improve-guess guess))))
    (iter guess)))

(defn average [x y]
  (/ (+ x y) 2))

(defn sqrt [x]
  ((iterative-improve (fn [guess]
                        (< (abs (- (square guess) x))
                           0.001))
                      (fn [guess]
                        (average guess (/ x guess))))
  1.0))

(defn fixed-point [f first-guess]
  ((iterative-improve (fn [guess]
                        (< (abs (- guess (f guess))) 0.00001))
                      (fn [guess]
                        (f guess)))
   first-guess))
