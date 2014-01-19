;; Implementation of Newton's method for cube roots.
;; (x/y^2 + 2y) / 3

(defn square [x]
  (* x x))

(defn improve [guess x]
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(defn delta [guess x]
  (abs (/ (- guess (improve guess x)) guess)))

(defn good-enough? [guess x]
  (< (delta guess x) 1e-10))

(defn cube-iter [guess x]
  (if (good-enough? guess x)
    guess
    (cube-iter (improve guess x) x)))

(defn cube-root [x]
  (cube-iter 1.0 x))

(cube-root 81)