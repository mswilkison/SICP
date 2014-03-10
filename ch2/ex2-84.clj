; types hierarchy:
; 0 - scheme-number
; 1 - rational
; 2 - complex

(defn tower-level [x]
  (let [type-x (type-tag x)]
    (cond (= type-x 'rational) 1
          (= type-x 'complex) 3
          :else 0)))

(defn raise-to [level x]
  (if (= level (tower-level x))
    x
    (raise-to level (raise x))))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)]
    (let [proc (get op type-tags)]
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let [a1 (first args)
                a2 (second args)
                (level1 (tower-level a1))
                (level2 (tower-level a2))]
              (cond (< level1 level2) (apply-generic op (raise-to level2 a1) a2)
                    (< level2 level1) (apply-generic op a1 (raise-to level1 a2))
                    :else (throw (Exception. "No method for these types" (list op type-tags)))))
          (throw (Exception. "No method for these types"
                             (list op type-tags))))))))
