(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (* (expmod base (/ exp 2) m)
                            (expmod base (/ exp 2) m))
                         m)
        :else (mod (* base (expmod base (- exp 1) m))
                   m)))

; Using explicit multiplication to square
; (expmod base (/ exp 2) m) causes it to be evaluated twice,
; rather than once as when the (square) function is used.
