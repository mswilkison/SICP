(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn reverse [items]
  (if (empty? (rest items))
    items
    (append (reverse (rest items)) [(first items)])))

(reverse '(1 4 9 16 25))
