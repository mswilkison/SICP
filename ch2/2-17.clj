(cons 1 [()])

(defn last-pair [items]
  (if (empty? (rest items))
    items
    (last-pair (rest items))))

(last-pair (list 23 72 149 34))
