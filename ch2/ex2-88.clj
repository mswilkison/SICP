(defn negate [x] (apply-generic 'negate x))

(put 'negate 'scheme-number (fn [x] (tag (- x))))
(put 'negate 'rational (fn [x] (make-rat (- (numer x)) (denom x))))
(put 'negate 'complex (fn [x] (make-from-real-imag (- (real-part x)) (- (imag-part x)))))

(defn negate-terms [L]
  (if (empty-termlist? L)
    (the-empty-list)
    (let [t (first-term L)]
      (adjoin-term (make-term (order t) (negate (coeff t)))
                   (negate-terms (rest-terms L))))))

(put 'negate 'polynomial (fn [x] (make-poly (variable x)
                                            (negate-terms (term-list x)))))

(put 'sub 'polynomial (fn [x y]
                        (tag (add-poly x (negate y)))))
