(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(defn enumerate-interval [low high]
  (if (> low high)
    ()
    (cons low (enumerate-interval (inc low) high))))

(defn flatmap [proc lst]
  (accumulate append () (map proc lst)))

(defn unique-pairs [n]
  (flatmap
   (fn [i]
     (map (fn [j] (list i j))
          (enumerate-interval 1 (dec i))))
   (enumerate-interval 1 n)))

(defn divides? [a b]
  (= (mod b a) 0))

(defn square [x]
  (* x x))

(defn smallest-divisor [n]
  (defn find-divisor [test-divisor]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (find-divisor (+ test-divisor 1))))
  (find-divisor 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (last pair))))

(defn make-pair-sum [pair]
  (list (first pair) (last pair) (+ (first pair) (last pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 6)
