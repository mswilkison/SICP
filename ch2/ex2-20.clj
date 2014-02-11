(defn evens [lst]
  (if (empty? lst)
    ()
    (if (even? (first lst))
      (cons (first lst) (evens (rest lst)))
      (evens (rest lst)))))

(defn odds [lst]
  (if (empty? lst)
    ()
    (if (odd? (first lst))
      (cons (first lst) (odds (rest lst)))
      (odds (rest lst)))))

(defn same-parity [x & args]
  (cons x
        (if (even? x)
          (evens args)
          (odds args))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
