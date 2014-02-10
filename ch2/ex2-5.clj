(defn cons [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

(defn factor-count [z d]
  (defn iter [x result]
    (if (= 0 (mod x d))
      (iter (/ x d) (inc result))
      result))
  (iter z 0))

(defn car [z]
  (factor-count z 2))
  ;(get-val (factor z 3) 2))

(defn cdr [z]
  (factor-count z 3))
  ;(get-val (factor z 2) 3))

(def z (cons 2 4))
z

(car z)
(cdr z)
