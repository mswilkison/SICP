; Recursive implementation
(defn accumulate [combiner null-value term a step b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (step a) step b))))

(accumulate + 0 identity 1 inc 10) ; Sum
(accumulate * 1 identity 1 inc 10) ; Product

; Iterative implementation
(defn accumulate [combiner null-value term a step b]
  (defn iter [a result]
    (if (> a b)
      result
      (combiner (term a) result (iter (step a) result))))
  (iter a null-value))

(accumulate + 0 identity 1 inc 10) ; Sum
(accumulate * 1 identity 1 inc 10) ; Product
