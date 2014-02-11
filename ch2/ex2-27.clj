(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn reverse [items]
  (if (empty? (rest items))
    items
    (append (reverse (rest items)) [(first items)])))

(def x (list (list 1 2) (list 3 4)))
x

(reverse x)

(defn deep-reverse [lst]
  (cond (empty? lst) ()
        (seq? (first lst)) (append (deep-reverse (rest lst))
                           (list (deep-reverse (first lst))))
        :else (append (deep-reverse (rest lst))
                      (list (first lst)))))

(deep-reverse x)
