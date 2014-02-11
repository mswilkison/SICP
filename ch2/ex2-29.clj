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

(defn torque [branch]
  (* (branch-length branch)
     (branch-weight branch)))

(defn balanced? [mobile]
  (if-not (seq? (branch-structure mobile))
    true
    (and (= (torque (left-branch mobile))
            (torque (right-branch mobile)))
         (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile))))))

(def unbalanced-mobile (make-mobile (list 1 z) x))
(def naively-balanced-mobile (make-mobile (list 2 z) (list x x)))
naively-balanced-mobile

(balanced? z)
(seq? (branch-structure z))
(branch-structure z)
(branch-structure (left-branch z))
(balanced? zeta)
(balanced? unbalanced-mobile)
