(defn element-of-set? [x set1]
  (cond (empty? set1) false
        (= x (first set1)) true
        :else (element-of-set? x (rest set1))))

(defn adjoin-set [x set1]
  (cons x set1))

(defn union-set [set1 set2]
  (if (empty? set1)
    set2
    (union-set (rest set1)
               (cons (first set1) set2))))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2) (cons (first set1)
                                                  (intersection-set (rest set1)
                                                                    set2))
        :else (intersection-set (rest set1) set2)))
