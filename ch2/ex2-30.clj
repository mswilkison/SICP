(defn square-tree-direct [tree]
  (cond (not (seq? tree)) (* tree tree)
        (empty? tree) ()
        :else (cons (square-tree-direct (first tree))
                    (square-tree-direct (rest tree)))))

(square-tree-direct (list 1
                          (list 2 (list 3 4) 5)
                          (list 6 7)))

(defn square-tree-map [tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (square-tree-map sub-tree)
           (* sub-tree sub-tree))) tree))

(square-tree-map (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))
