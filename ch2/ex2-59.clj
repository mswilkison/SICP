(defn element-of-set? [x my-set]
  (cond (empty? my-set) false
        (= x (first my-set)) true
        :else (element-of-set? x (rest my-set))))

(defn adjoin-set [x my-set]
  (if (element-of-set? x my-set)
    my-set
    (cons x my-set)))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2)
          (cons (first set1)
                (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))

(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        (element-of-set? (first set1) set2)
          (union-set (rest set1) set2)
        :else (cons (first set1)
                    (union-set (rest set1) set2))))
