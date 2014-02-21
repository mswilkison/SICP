(defn make-vect [x-cor y-cor]
  (cons x-cor [y-cor]))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (first (rest v)))

(defn bi-op-vect [op v1 v2]
  (cons (op (xcor-vect v1)
            (xcor-vect v2))
        [(op (ycor-vect v1)
             (ycor-vect v2))]))

(defn add-vect [v1 v2]
  (bi-op-vect + v1 v2))

(defn sub-vect [v1 v2]
  (bi-op-vect - v1 v2))

(defn scale-vect [s v]
  (cons (* s (xcor-vect v))
        [(* s (ycor-vect v))]))
