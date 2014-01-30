(defn cube [x]
  (* x x x))

(defn p [x]
  (println "call")
  (- (* 3 x) (* 4 (cube x))))

(defn abs [x]
  (if (>= x 0)
    x
    (- x)))

(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

; `p` is applied 5 times when (sine 12.15) is evaluated
; Space: O(log n)
; Steps: O(log n)
; Both space and steps grow linearly when input size is tripled.