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

; Exercise 2.65
; We can define an O(n) implementation of union-set using balanced binary trees
; by converting the trees into lists, using the relevant O(n) procedures we previously defined
; and then converting the result back into a tree.

(defn entry [tree] (first tree))
(defn left-branch [tree] (second tree))
(defn right-branch [tree] (last tree))

(defn tree->list-2 [tree]
  (defn copy-to-list [tree result-list]
    (if (empty? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(defn union-set [tree1 tree2]
  (defn union-list [set1 set2]
    (let [x1 (first set1)
          x2 (first set2)]
      (cond (empty? set1) set2
            (empty? set2) set1
            (= x1 x2) (cons x1 (union-set (rest set1)
                                          (rest set2)))
            (< x1 x2) (cons x1 (union-set (rest set1)
                                          set2))
            :else (cons x2 (union-set set1
                                      (rest set2))))))
  (list->tree (union-list (tree->list-2 tree1)
                          (tree->list-2 tree2))))

(defn intersection-set [tree1 tree2]
  (defn intersection-list [set1 set2]
    (if (or (empty? set1) (empty? set2))
      '()
      (let [x1 (first set1)
            x2 (first set2)]
        (cond (= x1 x2) (cons x1
                              (intersection-set (rest set1)
                                                (rest set2)))
              (< x1 x2) (intersection-set (rest set1)
                                          set2)
              (< x2 x1) (intersection-set set1
                                          (rest set2))))))
  (list->tree (intersection-list (tree->list-2 tree1)
                                 (tree->list-2 tree2))))
