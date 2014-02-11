(defn tree-map [proc tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree))) tree))

(defn square-tree [tree]
  (tree-map (fn [x] (* x x)) tree))

(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
