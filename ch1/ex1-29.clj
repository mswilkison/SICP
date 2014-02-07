(defn cube [x]
  (* x x x))

(defn sum [term a step b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (step a) step b))))


(defn simpson-integral [f a b n]
  (def h (/ (- b a) n))
  (defn coefficient [k]
    (cond (= k 0) 1
          (= k n) 1
          (= (mod k 2) 0) 2
          :else 4))
  (defn yk [k] (f (+ a (* k h))))
  (defn full-term [k]
    (* (coefficient k) (yk k)))
  (* (/ h 3)
     (sum full-term 0 inc n)))


(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
