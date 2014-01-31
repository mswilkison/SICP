(defn square [x]
  (* x x))

(defn divides? [a b]
  (= (mod b a) 0))

(defn next-divisor [test-divisor]
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (next-divisor test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(time (prime? 1009))
(time (prime? 1013))
(time (prime? 1019))
(time (prime? 10007))
(time (prime? 10009))
(time (prime? 10037))
(time (prime? 1000003))
(time (prime? 1000033))
(time (prime? 1000037))
