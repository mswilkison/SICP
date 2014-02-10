; Exercise 2.7
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

; Exercise 2.8
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9
(defn width [x]
  (/ (- (upper-bound x) (lower-bound x))
     2))

(def a (make-interval -5 10))
(def b (make-interval 7 16))

(width a)
(width b)
(width (add-interval a b))
(width (sub-interval a b))
; Width of the sum/difference of two intervals is equal to the sum of each interval's width

(width (mul-interval a b))
(width (div-interval a b))
(div-interval a b)
; Width of the product/quotient of two intervals is not a function of the input intervals' width

; Exercise 2.10
(defn span-zero? [x]
  (and (< (lower-bound x) 0)
       (> (upper-bound x) 0)))

(defn div-interval [x y]
  (if (span-zero? y)
    (throw (Exception. "Cannot divide by an interval that spans zero!"))
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))
