; Exercise 2.24
(list 1 (list 2 (list 3 4)))

; Exercise 2.25
(first (rest (first (rest (rest '(1 3 (5 7) 9))))))

(first (first '((7))))

(first (rest (first (rest (first (rest (first (rest (first (rest (first (rest '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

; Exercise 2.26
(def x '(1 2 3))
(def y '(4 5 6))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(append x y)
(cons x y)
(list x y)
