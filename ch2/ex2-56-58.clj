(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2)) (= v1 v2))

(defn =number? [exp number]
  (and (number? exp) (= exp number)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn sum? [x]
  (and (seq? x) (= (first x) '+)))

(defn addend [s] (second s))

(defn augend [s] (last s))

(defn product? [x]
  (and (seq? x) (= (first x) '*)))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (last p))

; Exercise 2.56

(defn exponentiation? [x]
  (and (seq? x) (= (first x) '**)))

(defn base [e]
  (second e))

(defn exponent [e]
  (last e))

(defn make-exponentiation [e1 e2]
  (cond (=number? e2 0) 1
        (=number? e2 1) e1
        :else (list '** e1 e2)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        (exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp)
                                                                   (make-sum (exponent exp) -1)))
                                (deriv (base exp) var))
        :else (throw (Exception. "unknown expression type -- DERIV" exp))))

; Exercise 2.57
(defn augend [s]
  (if (empty? (rest (rest (rest s))))
    (last s)
    (cons '+ (rest (rest s)))))

(defn multiplicand [p]
  (if (empty? (rest (rest (rest p))))
    (last p)
    (cons '* (rest (rest p)))))

(deriv '(* x y (+ x 3)) 'x)

; Exercise 2.58
; (a)
(defn sum? [x]
  (and (seq? x) (= (second x) '+)))

(defn product? [x]
  (and (seq? x) (= (second x) '*)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list m1 '* m2)))

(defn addend [s] (first s))

(defn multiplier [p]
  (first p))

(defn augend [s] (last s))

(defn multiplicand [p] (last p))

(deriv '(x + (3 * (x + (y + 2)))) 'x)

; (b)
(defn simplify [exp]
  (if (empty? (rest exp))
    (first exp)
    exp))

(defn augend [s] (simplify (rest (rest s))))

(defn multiplicand [p] (simplify (rest (rest p))))

(deriv '(x + 3 * (x + y + 2)) 'x)
