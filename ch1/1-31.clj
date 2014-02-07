;; Recursive implementation
(defn product [term a step b]
  (if (> a b)
    1
    (* (term a)
       (product term (step a) step b))))

(defn factorial [n]
  (product identity 1 inc n))

(defn make-even [a]
  (if (= (mod a 2) 0)
    a
    (+ a 1)))

(defn make-odd [a]
  (if-not (= (mod a 2) 0)
    a
    (- a 1)))

(defn approx-pi [n]
  (/ (product make-even 2 inc n)
     (product make-odd 3 inc (+ n 1))))

;; Iterative implementation
(defn product [term a step b]
  (defn iter [a result]
    (if (> a b)
      result
      (* (term a) (iter (step a) result))))
  (iter a 1))
