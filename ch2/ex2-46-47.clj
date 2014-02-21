; Exercise 2.46
(defn make-vect [x-cor y-cor]
  (cons x-cor [y-cor]))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (first (rest v)))

(defn bi-op-vect [op v1 v2]
  (make-vect (op (xcor-vect v1)
                 (xcor-vect v2))
             (op (ycor-vect v1)
                 (ycor-vect v2))))

(defn add-vect [v1 v2]
  (bi-op-vect + v1 v2))

(defn sub-vect [v1 v2]
  (bi-op-vect - v1 v2))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Exercise 2.47
(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn make-frame [origin edge1 edge2]
  (cons origin (cons edge1 [edge2])))

(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (first (rest frame)))

(defn edge2-frame [frame]
  (first (rest (rest frame))))

(def fr (make-frame (make-vect 0 0) (make-vect 1 1) (make-vect 5 5)))

(origin-frame fr)
(edge1-frame fr)
(edge2-frame fr)
