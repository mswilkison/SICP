(defn raise [x]
  (apply-generic 'raise x))

(put 'raise '(scheme-number) (fn [n] (make-rat n 1)))

(put 'raise '(rational) (fn [r] (make-complex-from-real-imag r 0)))
