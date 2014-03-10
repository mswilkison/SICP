; (a) It will get stuck in an infinite `apply-generic` loop

; (b) He's wrong. `apply-generic` will return false, as expected

; (c)
(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)]
    (let [proc (get op type-tags)]
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let [type1 (first type-tags)
                type2 (second type-tags)
                a1 (first args)
                a2 (second args)]
            (if-not (= type1 type2)
              (let [t1->t2 (get-coercion type1 type2)
                    t2->t1 (get-coercion type2 type1)]
                (cond (t1->t2) (apply-generic op (t1->t2 a1) a2)
                      (t2->t1) (apply-generic op a1 (t2->t1 a2))
                      :else (throw (Exception. "No method for these types"
                                               (list op type-tags)))))
              (throw (Exception. "No method for these types"
                                 (list op type-tags)))))
          (throw (Exception. "No method for these types"
                             (list op type-tags))))))))
