(defn square [x]
  (* x x))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (square (expmod base (/ exp 2) m)) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)))

; Modify to test every a where a < n
(defn fermat-test [n a]
  (= (expmod a n n) a))

(defn fermat-all [n]
  (defn iter [a]
    (cond (= a 1) true
          (not (fermat-test n a)) false
          :else (fermat-test n (- a 1))))
  (iter (- n 1)))

(fermat-all 561)
(fermat-all 1105)
(fermat-all 1729)
(fermat-all 2465)
(fermat-all 2821)
(fermat-all 6601)