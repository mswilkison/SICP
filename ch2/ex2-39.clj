(defn fold-right [op initial lst]
  (if (empty? lst)
    initial
    (op (first lst)
        (fold-right op initial (rest lst)))))

(defn fold-left [op initial lst]
  (defn iter [result remaining]
    (if (empty? remaining)
      result
      (iter (op result (first remaining))
            (rest remaining))))
  (iter initial lst))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn reverse-right [lst]
  (fold-right (fn [x y] (append y [x])) () lst))

(defn reverse-left [lst]
  (fold-left (fn [x y] (cons y x)) () lst))

(reverse-right '(1 2 3 4))
(reverse-left '(1 2 3 4))
