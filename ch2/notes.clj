;; Chapter 2

(defn linear-combination [a b x y]
  (+ (* a x) (* b y)))

(defn linear-combination [a b x y]
  (add (mult a x) (mult b y)))

;; 2.1 Introduction to Data Abstraction

; 2.1.1 Example: Arithmetic Operations for Rational Numbers
(defn make-rat [n d] (cons n [d]))

(defn numer [x] (first x))

(defn denom [x] (last x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def x [1 2])
(first x)
(last x)

(def x [1 2])
(def y [3 4])
(def z (cons x [y]))
z
(first (first z))
(first (last z))

(defn print-rat [x]
  (newline)
  (print (numer x))
  (print "/")
  (print (denom x)))

(def one-half (make-rat 1 2))
(print-rat one-half)

(def one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons (/ n g) [(/ d g)])))

(print-rat (add-rat one-third one-third))

(defn make-rat [n d]
  (cons n [d]))

(defn numer [x]
  (let [g (gcd (first x) (last x))]
    (/ (first x) g)))

(defn denom [x]
  (let [g (gcd (first x) (last x))]
    (/ (last x) g)))

; 2.1.3 What Is Meant by Data?

;(defn cons [x y]
; (defn dispatch [m]
;    (cond (= m 0) x
;          (= m 1) y
;          :else (throw (Exception. (str "Argument not 0 or 1 -- CONS" m)))))
;  dispatch)

(defn car [z] (z 0))

(defn cdr [z] (z 1))

; 2.1.4 Extended Exercise: Interval Arithmetic
(defn make-interval [a b] (cons a [b]))

(defn upper-bound [interval]
  (last interval))

(defn lower-bound [interval]
  (first interval))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
