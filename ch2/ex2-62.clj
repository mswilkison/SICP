(defn union-set [set1 set2]
  (let [x1 (first set1)
        x2 (first set2)]
    (cond (empty? set1) set2
          (empty? set2) set1
          (= x1 x2) (cons x1 (union-set (rest set1)
                                        (rest set2)))
          (< x1 x2) (cons x1 (union-set (rest set1)
                                        set2))
          :else (cons x2 (union-set set1
                                    (rest set2))))))
