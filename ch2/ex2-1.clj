; gcd returns the appropriate sign

(defn make-rat [n d]
  (defn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b))))
  (let [g (gcd n d)]
    (cons (/ n g) [(/ d g)])))
