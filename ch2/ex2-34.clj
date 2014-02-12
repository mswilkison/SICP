(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
