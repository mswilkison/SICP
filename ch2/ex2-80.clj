; generic operation
(defn =zero? [x]
  (apply-generic '=zero? x))

; scheme-numbers
(put '=zero? '(scheme-numbers scheme-numbers) (fn [x] (= 0 x)))

; rational
(put '=zero? '(rational rational) (fn [x] (= 0 (numer x))))

; complex
(put '=zero? '(complex complex) (fn [x] (= 0 (magnitude x))))
