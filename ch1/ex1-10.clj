; Ackermann's function
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(A 1 10)
(A 2 4)
(A 3 3)


; `f` == 2 * n
(defn f [n] (A 0 n))

; `g` == 2^n
(defn g [n] (A 1 n))

; 'h' == 2^(n+1)
(defn h [n] (A 2 n))