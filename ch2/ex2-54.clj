(defn equal? [lst1 lst2]
  (cond (and (symbol? lst1) (symbol? lst2)) (= lst1 lst2)
        (and (empty? lst1) (empty? lst2)) true
        (and (seq? lst1) (seq? lst2)) (and (= (first lst1)
                                              (first lst2))
                                           (equal? (rest lst1)
                                                   (rest lst2)))
        :else false))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
