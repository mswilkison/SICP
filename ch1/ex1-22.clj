(defn square [x]
  (* x x))

(defn divides? [a b]
  (= (mod b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

; Find first three prime numbers starting at pos

(defn prime-found [pos n]
  (println (str "PRIME FOUND: " pos))
  (search-for-primes (+ pos 2) (+ n 1)))

(defn search-for-primes [pos n]
  (if (< n 3)
    (if (even? pos)
      (if (time (prime? (+ pos 1)))
        (prime-found (+ pos 1) n)
        (search-for-primes (+ pos 2) n))
      (if (time (prime? pos))
        (prime-found pos n)
        (search-for-primes (+ pos 2) n)))))

(search-for-primes 1000 0)
(println "------------")
(search-for-primes 10000 0)
(println "------------")
(search-for-primes 1000000 0)
(println "------------")