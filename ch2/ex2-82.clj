(defn apply-generic [op & args]
  (defn try-convert [x new-type]
    (let [converter (get-coercion (type-tag x) new-type)]
      (if converter
        (converter x)
        x)))
  (defn apply-generic-1 [op args type-list]
    (if (empty? type-list)
      (throw (Exception. "No method for these types"
                         (list op type-list)))
      (let [new-args (map (fn [x]
                            (try-convert x (first type-list))
                            args))]
        (let [new-type-tags (map type-tag new-args)]
          (let [proc (get op new-type-tags)]
            (if proc
              (apply proc (map contents new-args))
              (apply-generic-1 op args (rest type-list))))))))
  (let [type-tags (map type-tag args)]
    (let [proc (get op type-tags)]
      (if proc
        (apply proc (map contents args))
        (apply-generic-1 op args type-tags)))))
