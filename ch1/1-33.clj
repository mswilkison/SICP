(defn square [x]
  (* x x))

(defn divides? [a b]
  (= (mod b a) 0))

(defn smallest-divisor [n]
  (defn find-divisor [test-divisor]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (find-divisor (+ test-divisor 1))))
  (find-divisor 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn filtered-accumulate [predicate combiner null-value term a step b]
  (cond (> a b) null-value
        (predicate a) (combiner (term a)
                                (filtered-accumulate predicate combiner null-value term (step a) step b))
        :else (filtered-accumulate predicate combiner null-value term (step a) step b)))

(defn sum-prime-squares [a b]
  (filtered-accumulate prime? + 0 square a inc b))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(defn multiply-relative-primes [n]
  (defn relative-prime? [i]
    (= (gcd i n) 1))
  (filtered-accumulate relative-prime? * 1 identity 1 inc (- n 1)))
