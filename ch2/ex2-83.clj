(defn raise [x] (apply-generic 'raise x))

(put 'raise 'scheme-number (fn [x] (make-rat x 1)))
(put 'raise 'rational (fn [x] (make-real (/ (numer x) (denom x)))))
(put 'raise 'real (fn [x] (make-from-real-imag x 0)))


; Exercise 2.84
(defn apply-generic [op & args]
  (defn raise [subject target]
    (let [subject-type (type-tag subject)
          target-type (type-tag target)]
      (cond (= subject-type target-type) subject
            ((get 'raise (list subject-type))
             (raise ((get 'raise (list subject-type)) (contents subject)) target))
            :else false)))

  (let [type-tags (map type-tag args)]
    (let [proc (get op type-tags)]
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let [a1 (first args)]
            (cond
              (raise a1 a2) (apply-generic op (raise a1 a2) a2)
              (raise a2 a1) (apply-generic op a1 (raise a2 a1))
              :else (throw (Exception. "No method for these types"
                                       (list op type-tags)))))
          (throw (Exception. "No method for these types"
                             (list op type-tags))))))))
