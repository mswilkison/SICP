(defn expt [b p]
  (if (= p 0) 1
    (* b (expt b (dec p)))))

(defn cons [a b]
  (* (expt 2 a) (expt 3 b)))

(defn factor-count [z d]
  (defn iter [x result]
    (if (= 0 (mod x d))
      (iter (/ x d) (inc result))
      result))
  (iter z 0))

(defn car [z]
  (factor-count z 2))

(defn cdr [z]
  (factor-count z 3))

(def z (cons 5 6))

(car z)
(cdr z)
