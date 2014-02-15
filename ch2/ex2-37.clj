(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [x] (dot-product v x)) m))

(defn transpose [mat]
  (accumulate-n cons () mat))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [x] (matrix-*-vector cols x)) m)))

(def m1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(def v1 '(1 2 3 4))
(matrix-*-vector m1 v1)
(transpose m1)

(def mat1 '((0 3 5) (5 5 2)))
(def mat2 '((3 4) (3 -2) (4 -2)))
(matrix-*-matrix mat1 mat2)
