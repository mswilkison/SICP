; Russian peasant multiplication
; Progressively halve the left multiplicand (discarding the remainder)
; until you reach 1, while doubling the right multiplicand.
; Finally, sum the doubled right numbers, excluding any in which the corresponding
; left number is even.

(defn halve [x]
  (int (/ x 2)))

(defn double [y]
  (+ y y))

(defn * [x y]
  (cond (= x 1) y
        (even? x) (+ 0 (* (halve x) (double y)))
        :else (+ y (* (halve x) (double y)))))

(* 5 1)