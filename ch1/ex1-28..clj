(defn square [x]
  (* x x))

(defn non-trivial-square [a n]
  (if (and (not (or (= a 1) (= a (- n 1))))
           (= (mod (square a) n) 1))
    0
    (mod (square a) n)))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (non-trivial-square (expmod base (/ exp 2) m) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)))

(defn miller-rabin-test [n]
  (defn try-it [a]
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (rand-int (- n 2)))))

; test Carmichael numbers
(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)
