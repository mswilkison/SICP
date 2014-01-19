(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; Using `new-if` to compute square roots
;; will cause the function to infinitely recurse
;; on the else clause. This happens because
;; `new-if` is a function and when a function
;; is called, all the arguments are evaluated.