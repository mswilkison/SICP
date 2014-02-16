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

(defn unique-triples [n]
  (flatmap (fn [i]
             (flatmap (fn [j]
                        (map (fn [k] (list i j k))
                             (enumerate-interval 1 (dec j))))
                      (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(defn sum-triple [triple]
  (+ (first triple)
     (second triple)
     (last triple)))

(defn sum-to-s-triples [n s]
  (filter (fn [x] (= (sum-triple x) s)) (unique-triples n)))

(unique-triples 6)
(sum-to-s-triples 6 10)
