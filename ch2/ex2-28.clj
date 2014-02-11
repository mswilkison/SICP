(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn fringe [lst]
  (cond (empty? lst) lst
        (seq? (first lst)) (append
                            (fringe (first lst))
                            (fringe (rest lst)))
        :else (append (list (first lst))
                      (fringe (rest lst)))))

(def x (list (list 1 2) (list 3 4)))
x

(fringe x)
(fringe (list x x))
