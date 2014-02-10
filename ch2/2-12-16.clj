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

; Exercise 2.14
(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 a b)
(par2 a b)
(par1 (make-center-percent 1 1) (make-center-percent 1 1))
(par2 (make-center-percent 1 1) (make-center-percent 1 1))

(center (div-interval a a))
(center (div-interval a b))

; Exercise 2.15
; The reason for the discrepancy has to do with the inherent imprecision in the computer's
; representation of floating-point numbers. Repeated operations on an imprecise number
; will compound the imprecision. In this case, Eva Lu Ator is correct.

; Exercise 2.16
; We cannot create a library without this shortcoming, but we can avoid repeating variables
; to the extent possible so as to minimize the error.
