(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn entry [tree] (first tree))
(defn left-branch [tree] (second tree))
(defn right-branch [tree] (last tree))

(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x set1]
  (cond (empty? set1) false
        (= x (entry set1)) true
        (< x (entry set1)) (element-of-set? x
                                            (left-branch set1))
        (> x (entry set1)) (element-of-set? x
                                            (right-branch set1))))

(defn adjoin-set [x set1]
  (cond (empty? set1) (make-tree x '() '())
        (= x (entry set1)) set1
        (< x (entry set1)) (make-tree (entry set1)
                                      (adjoin-set x (left-branch set1))
                                      (right-branch set1))
        (> x (entry set1)) (make-tree (entry set1)
                                      (left-branch set1)
                                      (adjoin-set x (right-branch set1)))))

(defn tree->list-1 [tree]
  (if (empty? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (defn copy-to-list [tree result-list]
    (if (empty? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(def tree1 (make-tree 7
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9 '() (make-tree 11 '() '()))))
(def tree2 (make-tree 3
                      (make-tree 1 '() '())
                      (make-tree 7
                                 (make-tree 5 '() '())
                                 (make-tree 9 '() (make-tree 11 '() '())))))
(def tree3 (make-tree 5
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 '())
                      (make-tree 9
                                 (make-tree 7 '() '())
                                 (make-tree 11 '() '()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

; Both procedures produce an ordered list
; tree->list-2 grows at O(n). It visits each entry and conses it to list-result
; tree->list-1 growsn at O(nlogn).
