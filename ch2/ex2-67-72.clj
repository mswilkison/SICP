(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn make-leaf [sym weight]
  (list 'leaf sym weight))

(defn leaf? [object]
  (= (first object) 'leaf))

(defn symbol-leaf [x] (second x))

(defn weight-leaf [x] (last x))

(defn left-branch [tree]
  (first tree))

(defn right-branch [tree]
  (second tree))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (nth tree 2)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (last tree)))

(defn make-code-tree [left right]
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (throw (Exception. "bad bit -- CHOOSE-BRANCH" bit))))

(defn decode [bits tree]
  (defn decode-1 [bits current-branch]
    (if (empty? bits)
      '()
      (let [next-branch (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (rest bits) tree))
          (decode-1 (rest bits) next-branch)))))
  (decode-1 bits tree))

(defn adjoin-set [x set1]
  (cond (empty? set1) (list x)
        (< (weight x) (weight (first set1))) (cons x set1)
        :else (cons (first set1)
                    (adjoin-set x (rest set1)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair)
                             (second pair))
                  (make-leaf-set (rest pairs))))))

; Exercise 2.67
(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; Exercise 2.68
(defn element-of-set? [x my-set]
  (cond (empty? my-set) false
        (= x (first my-set)) true
        :else (element-of-set? x (rest my-set))))

(defn encode-symbol [letter branch]
  (cond (leaf? branch) '()
        (element-of-set? letter (symbols (left-branch branch)))
          (append '(0) (encode-symbol letter (left-branch branch)))
        (element-of-set? letter (symbols (right-branch branch)))
          (append '(1) (encode-symbol letter (right-branch branch)))
        :else (throw (Exception. "symbol not contained in tree" letter))))

(defn encode [message tree]
  (if (empty? message)
    '()
    (append (encode-symbol (first message) tree)
            (encode (rest message) tree))))

(encode '(A D A B B C A) sample-tree)

;; Exercise 2.69
(defn generate-huffman-tree [pairs]
  (defn successive-merge [leaf-set]
    (if (empty? (rest leaf-set))
      (first leaf-set)
      (let [next-node (make-code-tree (first leaf-set) (second leaf-set))]
        (successive-merge (adjoin-set next-node (rest (rest leaf-set)))))))
  (successive-merge (make-leaf-set pairs)))

(def sample-pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
(make-leaf-set sample-pairs)
(generate-huffman-tree sample-pairs)

;; Exercise 2.70
(def rel-freqs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(def rock-tree (generate-huffman-tree rel-freqs))
(def encoded-song (encode '(GET A JOB
                            SHA NA NA NA NA NA NA NA NA
                            GET A JOB
                            SHA NA NA NA NA NA NA NA NA
                            WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                            SHA BOOM)
                          rock-tree))
(count encoded-song) ; bits needed for Huffman encoding
(Math/pow 2 3) ; 8 symbols to encode, so we need 3 bits per symbol
(* 36 3) ; 36 symbols total, so 108 bits are needed for fixed-length encoding

;; Exercise 2.71
;********n=5********
;       +
;      / \
;     16  \
;          +
;         / \
;        8   \
;             +
;            / \
;           4   \
;                +
;               / \
;              2   1

;*******n=10********
;       +
;      / \
;   512   +
;        / \
;     256   +
;          / \
;       128   +
;            / \
;          64   +
;              / \
;            32   +
;                / \
;              16   +
;                  / \
;                 8   +
;                    / \
;                   4   +
;                      / \
;                     2   1

; 1 bit is required to encode the most frequent symbol
;(n-1) bits are needed to encode the least frequent symbol
