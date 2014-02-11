(defn square-list [items]
  (if (empty? items)
    ()
    (cons ((fn [x] (* x x)) (first items))
          (square-list (rest items)))))

(defn square-list-2 [items]
  (map (fn [x] (* x x)) items))

(square-list '(1 2 3 4))
(square-list-2 '(1 2 3 4))
