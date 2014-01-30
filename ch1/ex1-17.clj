; Fast multiplication using only addition, double, and halve

(defn double [x]
  (+ x x))

(defn halve [x]
  (/ x 2))

(defn * [a b]
  (cond (= b 0) 0
        (even? b) (double (* a (halve b)))
        :else (+ a (* a (- b 1)))))