; Exercise 2.12
(defn make-interval [a b] (cons a [b]))

(defn upper-bound [interval]
  (last interval))

(defn lower-bound [interval]
  (first interval))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn make-center-percent [c p]
  (make-center-width c (/ p 100.0)))

(defn percent [i]
  (* 100 (/ (width i) (center i))))
