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

; Exercise 2.13
(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(def a (make-center-percent 1 5))
(def b (make-center-percent 1 2))

(percent (mul-interval a b))
; The tolerance of the product of two intervals is approximately
; the sum of the tolerance's of the two intervals
