(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (first (rest mobile)))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (first (rest branch)))

(defn branch-weight [branch]
  (let [struc (branch-structure branch)]
    (println struc)
    (if (seq? struc)
      (total-weight struc)
      struc)))

(defn total-weight [mobile]
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(def x (list 1 1))
(def y (list 1 1))
(def z (make-mobile x y))
(def zeta (make-mobile (list 1 z) (list 1 z)))

(total-weight z)
(total-weight zeta)
