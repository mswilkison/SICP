(defn accumulate [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (accumulate op initial (rest lst)))))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn enumerate-tree [tree]
  (cond (not (seq? tree)) (list tree)
        (empty? tree) ()
        :else (append (enumerate-tree (first tree))
                      (enumerate-tree (rest tree)))))

(defn count-leaves [t]
  (accumulate + 0 (map (fn [x] 1) (enumerate-tree t))))

(def x (cons (list 1 2) (list 3 4)))
(count-leaves x)
(count-leaves (list x x))
