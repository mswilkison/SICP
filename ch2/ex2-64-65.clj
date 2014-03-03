(defn make-tree [entry left right]
  (list entry left right))

; Exercise 2.64

(defn partial-tree [elements n]
  (if (= n 0)
    (cons '() elements)
    (let [left-size (quot (dec n) 2)]
      (let [left-result (partial-tree elements left-size)]
        (let [left-tree (first left-result)
              non-left-elements (rest left-result)
              right-size (- n (inc left-size))]
          (let [this-entry (first non-left-elements)
                right-result (partial-tree (rest non-left-elements)
                                           right-size)]
            (let [right-tree (first right-result)
                  remaining-elements (rest right-result)]
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elements))))))))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(list->tree '(1 3 5 7 9 11))


; partial-tree is called recursively, dividing the list into 3 sections: a center that is to be
; the parent node, smaller elements, and larger elements which will become the left-branch
; and right-branch, respectively. The smaller and larger segments are passed to partial-tree to
; recursively construct the branches.
; The order of growth for list->tree is O(n).
