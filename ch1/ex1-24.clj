(defn square [x]
  (* x x))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (square (expmod base (/ exp 2) m)) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int n))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

(time (fast-prime? 1009 1))
(time (fast-prime? 1013 1))
(time (fast-prime? 1019 1))
(time (fast-prime? 10007 1))
(time (fast-prime? 10009 1))
(time (fast-prime? 10037 1))
(time (fast-prime? 1000003 1))
(time (fast-prime? 1000033 1))
(time (fast-prime? 1000037 1))