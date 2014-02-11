(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (first (rest mobile)))

(def lb (left-branch z))
(def rb (right-branch z))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (first (rest branch)))
