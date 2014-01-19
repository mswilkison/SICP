;; `good-enough?` fails for very small numbers since the
;; tolerance threshold is set to 0.001. Thus, the test will
;; always pass if `x` is sufficiently small.

;; `good-enough?` fails for very large numbers due to the
;; lack of precision never results in a floating point number
;; within the necessary level of precision.

;; An alternative implementation of `good-enough?` that
;; examines the percentage change in `guess` from one iteration
;; to the next will better accomodate very small/large numbers.

(defn abs [x]
  (if (< x 0) (- x)
    x))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn delta [guess x]
  (abs (/ (- guess (improve guess x)) guess)))

(defn good-enough? [guess x]
  (< (delta guess x) 1e-10))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(defn square [x]
  (* x x))

(sqrt 0.00001)

(sqrt 1e100)
