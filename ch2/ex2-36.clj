(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    ()
    (cons (accumulate op init (map (fn [x] (first x)) seqs))
          (accumulate-n op init (map (fn [x] (rest x)) seqs)))))

(def x '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
x

(accumulate-n + 0 x)
